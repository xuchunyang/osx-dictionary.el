// ============================================================================
// Author: itchyny
// URL: https://github.com/itchyny/dictionary.vim/blob/59a818b62990aecb0ab0a18b596ccd1ef5bb5eb2/autoload/dictionary.m
// License: MIT License
// Build: clang -framework CoreServices -framework Foundation osx-dictionary.m -o osx-dictionary-cli
// Use: osx-dictionary-cli WORD                    -- search every dictionary active in Dictionary.app
//      osx-dictionary-cli -d NAME WORD            -- search only the dictionary named NAME
//      osx-dictionary-cli -d NAME -d NAME2 ... WORD -- search only the union of the named dictionaries,
//                                                     each block labeled \x01NAME\x01 on its own line
//      osx-dictionary-cli -l                      -- list installed dictionaries by name (for -d)
// ============================================================================

#import <Foundation/Foundation.h>
#import <CoreServices/CoreServices.h>

#define isnum(x)\
  (('0' <= x && x <= '9'))
#define isalpha(x)\
  (('a' <= x && x <= 'z') || ('A' <= x && x <= 'Z'))
#define tolower(x)\
  (('A' <= x && x <= 'Z') ? ((x) - ('A' - 'a')) : (x))

extern DCSDictionaryRef DCSGetDefaultDictionary(void);
extern CFArrayRef DCSCopyRecordsForSearchString(DCSDictionaryRef dict, CFStringRef string, void*, void*);
extern CFStringRef DCSRecordCopyData(CFTypeRef record, long version);
extern CFArrayRef DCSCopyAvailableDictionaries(void);
extern CFStringRef DCSDictionaryGetName(DCSDictionaryRef dictionary);

NSString* dictionary(char* searchword) {
  NSString* word = [NSString stringWithUTF8String:searchword];
  return (NSString*)DCSCopyTextDefinition(NULL, (CFStringRef)word,
                                          CFRangeMake(0, [word length]));
}

// Unique, non-empty definition texts the union of DICTS returns for WORD.
NSArray* recordsForDictionaries(NSArray* dicts, NSString* word) {
  NSMutableArray* results = [NSMutableArray array];
  NSMutableSet* seen = [NSMutableSet set];
  for (id dict in dicts) {
    if (!dict) continue;
    NSArray* records = (NSArray*)DCSCopyRecordsForSearchString((DCSDictionaryRef)dict, (CFStringRef)word, NULL, NULL);
    if (!records) continue;
    for (id record in records) {
      NSString* data = (NSString*)DCSRecordCopyData((CFTypeRef)record, 3);
      if (data && [data length] > 0 && ![seen containsObject:data]) {
        [seen addObject:data];
        [results addObject:data];
      }
    }
  }
  return results;
}

NSArray* recordsForDictionary(DCSDictionaryRef dict, NSString* word) {
  if (!dict) return [NSArray array];
  return recordsForDictionaries(@[(id)dict], word);
}

NSArray* dictionaryAll(NSString* word) {
  return recordsForDictionary(DCSGetDefaultDictionary(), word);
}

// Installed dictionary named NAME, or NULL. Dictionary objects come straight
// from DCSCopyAvailableDictionaries, never reconstructed via DCSDictionaryCreate
// (that path -- reading the active-dictionaries default + rebuilding a ref from
// its file URL -- is what broke on Sierra; see git history).
DCSDictionaryRef dictionaryNamed(const char* name) {
  NSString* target = [NSString stringWithUTF8String:name];
  for (id dict in (NSArray*)DCSCopyAvailableDictionaries()) {
    if ([(NSString*)DCSDictionaryGetName((DCSDictionaryRef)dict) isEqualToString:target])
      return (DCSDictionaryRef)dict;
  }
  return NULL;
}

void listDictionaries(void) {
  for (id dict in (NSArray*)DCSCopyAvailableDictionaries()) {
    NSString* name = (NSString*)DCSDictionaryGetName((DCSDictionaryRef)dict);
    if (name) printf("%s\n", [name UTF8String]);
  }
}

NSString* suggest(char* w) {
#define SORTEDSIZE 200
#define WORDLENGTH 50
#define HEADARG 25
  char format[] = "look %s|head -n %d", command[512],
       format_[] = "look %c%c|grep '%s'|head -n %d",
       output[WORDLENGTH], *ptr, all[HEADARG][WORDLENGTH], *sorted[SORTEDSIZE];
  int length[HEADARG], i, j = 0;
  FILE* fp;
  NSString* result;
  if (w[0] == '^') sprintf(command, format_, w[1], w[4], w, HEADARG);
  else             sprintf(command, format, w, HEADARG);
  if ((fp = popen(command, "r")) == NULL) return nil;
  for (i = 0; i < HEADARG; ++i) { all[i][0] = '\0'; length[i] = 0; }
  for (i = 0; i < SORTEDSIZE; ++i) sorted[i] = NULL;
  while (fgets(output, WORDLENGTH, fp) != NULL) {
    if (j >= HEADARG) break;
    if (isalpha(output[0])) {
      strcpy(all[j], output);
      length[j] = strlen(all[j]);
      if ((ptr = strchr(all[j++], '\n')) != NULL) *ptr = '\0';
      else length[j - 1] = 0;
    }
  }
  pclose(fp); ptr = NULL;
  for (i = 0; i < j; ++i) {
    j = length[i] * 6 - 6;
    if (0 < j && j < SORTEDSIZE) {
      while (sorted[j] != NULL) ++j;
      sorted[j] = all[i];
      if (ptr == NULL) ptr = sorted[j];
    }
  }
  for (i = 0 ; i < SORTEDSIZE; ++i)
    if (sorted[i] != NULL && sorted[i][0] != '\0' &&
       (result = dictionary(sorted[i])) != nil) return result;
  return nil;
}

void format_and_print(const char* r, int len, const char* word, int arglen) {
  if (len < 1) return;
  char s[len * 2];
  int i, j;
  char nr1[] = { -30, -106, -72 };
  char nr2[] = { -30, -106, -74 };
  char nr3[] = { -30, -128, -94 };
  char nr4[] = { -17, -67, -98, -52, -127 };
  char nr5[] = { -17, -67, -98, -52, -128 };
  char paren1[] = { -29, -128, -106 };
  char paren2[] = { -29, -128, -105 };
  char paren3[] = { -29, -128, -104 };
  int num = 0, newnum = 0;
  char al = 'a';
  char C = 0, U = 0, slash = 0;
  char paren = 1;
  char firstparen = 1;
  i = j = 0;
  if (arglen + 9 < len) {
    char flg = 1;
    for (i = 0; i < arglen; ++i) {
      if (tolower(r[i]) != tolower(word[i])) {
        flg = 0; break;
      }
    }
    if (flg) {
      if (r[i] == '.' || strncmp(r + i, paren3, 3) == 0) {
        for (i = 0; i < arglen + (r[i] == '.'); ++i)
          s[j++] = r[i];
        s[j++] = '\n';
      } else {
        i = j = 0;
      }
    } else {
      i = j = 0;
    }
  }
  for ( ; i < len; ++i, ++j) {
    if (strncmp(r + i, nr1, 3) == 0 || strncmp(r + i, nr3, 3) == 0) {
      if (j && s[j - 1] == '\n') --j;
      else s[j] = '\n';
      s[++j] = ' ';
      s[++j] = ' ';
      s[++j] = r[i];
      s[++j] = r[++i];
      s[++j] = r[++i];
    } else if (strncmp(r + i, nr2, 3) == 0) {
      s[j] = '\n';
      i += 2;
    } else if (strncmp(r + i, nr4, 5) == 0 || strncmp(r + i, nr5, 5) == 0) {
      s[j] = '\n';
      s[++j] = ' ';
      s[++j] = ' ';
      s[++j] = r[i];
      s[++j] = r[++i];
      s[++j] = r[++i];
      s[++j] = r[++i];
      s[++j] = r[++i];
    } else if (i + 3 < len && (r[i] == -30 && r[i + 1] == -111 && -97 < r[i + 2] && r[i + 2] < -76)) {
      s[j] = '\n';
      s[++j] = ' ';
      s[++j] = r[i];
      s[++j] = r[++i];
      s[++j] = r[++i];
    } else if (i + 3 < len && !isalpha(r[i]) && isalpha(r[i + 1]) && r[i + 2] == '.') {
      if (r[i + 1] == al + 1 || r[i + 1] == 'a' || r[i + 1] == 'A') {
        s[j] = r[i];
        s[++j] = '\n';
        s[++j] = ' ';
        al = s[++j] = r[++i];
        s[++j] = r[++i];
      } else {
        s[j] = r[i];
      }
    } else if (strncmp(r + i, "DERIVATIVES", 11) == 0 ||
               strncmp(r + i, "PHRASES", 7) == 0 ||
               strncmp(r + i, "ORIGIN", 6) == 0) {
      s[j] = '\n';
      s[++j] = r[i];
    } else if (i + 3 < len && isnum(r[i]) && isnum(r[i + 1]) && r[i + 2] == ' ' && 0 < i && !isnum(r[i - 1])) {
      newnum = (r[i] - '0') * 10 + (r[i + 1] - '0');
      if (0 < newnum && (num < newnum || newnum < 2) && newnum <= num + 2) {
        if (j > 1 && s[j - 1] != '\n') {
          s[j] = '\n';
          s[++j] = r[i];
          s[++j] = r[++i];
        } else {
          s[j] = r[i];
        }
        num = newnum;
        if (i + 3 < len && (r[i + 2] == 'C' || r[i + 2] == 'U')) {
          s[++j] = r[++i];
          C = U = 1;
          while ((C && r[i + 1] == 'C') || (U && r[i + 1] == 'U')) {
            s[++j] = r[++i];
            if (r[i] == 'C') C = 0;
            if (r[i] == 'U') U = 0;
          }
          s[++j] = ' ';
        }
      } else {
        s[j] = r[i];
      }
    } else if (r[i] == '/') {
      if (slash == 1 && i + 1 < len && r[i + 1] != '\n' && firstparen != 2) {
        s[j] = r[i];
        s[++j] = '\n';
        if (r[i + 1] == ' ')
          ++i;
      } else {
        s[j] = r[i];
      }
      slash++;
    } else if (i + 2 < len && isnum(r[i]) && r[i + 1] == ' ' && 0 < i && !isnum(r[i - 1])) {
      newnum = r[i] - '0';
      if (0 < newnum && (num < newnum || newnum < 2) && newnum <= num + 2) {
        if (j > 1 && s[j - 1] != '\n') {
          s[j] = '\n';
          s[++j] = r[i];
        } else {
          s[j] = r[i];
        }
        num = newnum;
        if (i + 3 < len && (r[i + 2] == 'C' || r[i + 2] == 'U')) {
          s[++j] = r[++i];
          C = U = 1;
          while ((C && r[i + 1] == 'C') || (U && r[i + 1] == 'U')) {
            s[++j] = r[++i];
            if (r[i] == 'C') C = 0;
            if (r[i] == 'U') U = 0;
          }
          s[++j] = ' ';
        }
      } else {
        s[j] = r[i];
      }
    } else if (!num && paren && strncmp(r + i, paren1, 3) == 0 && j > 2 && s[j - 1] != '\n' && s[j - 2] != '\n') {
      s[j] = '\n';
      s[++j] = r[i];
      s[++j] = r[++i];
      s[++j] = r[++i];
    } else if (!num && paren && strncmp(r + i, paren2, 3) == 0) {
      s[j] = r[i];
      s[++j] = r[++i];
      s[++j] = r[++i];
      s[++j] = '\n';
      paren = 0;
    } else if (firstparen == 1 && r[i] == '(') {
      s[j] = r[i];
      firstparen = 2;
    } else {
      s[j] = r[i];
    }
  }
  s[j] = '\0';
  printf("%s", s);
}

int main(int argc, char *argv[]) {
  if (argc < 2) return 0;

  if (strcmp(argv[1], "-l") == 0) {
    listDictionaries();
    return 0;
  }

  // Parallel arrays: names[k] is the display name of dicts[k].
  NSMutableArray* names = [NSMutableArray array];
  NSMutableArray* dicts = [NSMutableArray array];
  int i = 1;
  while (i + 1 < argc && strcmp(argv[i], "-d") == 0) {
    DCSDictionaryRef dict = dictionaryNamed(argv[i + 1]);
    if (dict) {
      [names addObject:[NSString stringWithUTF8String:argv[i + 1]]];
      [dicts addObject:(id)dict];
    }
    i += 2;
  }
  BOOL restricted = i > 1;
  if (i >= argc) return 0;  // no word given (only -d NAME pairs, or nothing at all)
  char* word = argv[i];

  int arglen = strlen(word);
  if (arglen == 0) return 0;

  NSString* nsword = [NSString stringWithUTF8String:word];

  if (restricted) {
    // Label every block with its source dictionary's name, always -- even
    // a single -d. Emacs turns \x01NAME\x01 lines into a heading; see
    // osx-dictionary--insert-search-result. A blank line separates each
    // dictionary's heading from the previous one's content, except the
    // very first (nothing to separate it from).
    int printed = 0;
    for (NSUInteger k = 0; k < [dicts count]; k++) {
      NSArray* results = recordsForDictionary((DCSDictionaryRef)dicts[k], nsword);
      BOOL first_of_dict = YES;
      for (NSString* result in results) {
        const char* r = [result UTF8String];
        int len = (int)strlen(r);
        if (len < 1) continue;
        if (first_of_dict) {
          if (printed > 0) printf("\n\n");
          printf("\x01%s\x01\n", [names[k] UTF8String]);
        } else if (printed > 0) {
          printf("\n--------------------\n");
        }
        first_of_dict = NO;
        format_and_print(r, len, word, arglen);
        printed++;
      }
    }
    // Nothing found in the restricted set: don't fall back to the fuzzy
    // `look'-based suggestion below, which ignores the restriction.
    return 0;
  }

  NSArray* results = dictionaryAll(nsword);

  if ([results count] == 0) {
    int i, l;
    if ((l = strlen(word)) > 100) return 0;
    for (i = 0; i < l; ++i)
      if (!isalpha(word[i])) return 0;
    NSString* result;
    if ((result = suggest(word)) == nil) {
      if (l < 3) return 0;
      int j; char s[l * 3 + 2]; s[0] = '^';
      for (i = j = 0; i < l; ++i) {
        s[++j] = word[i]; s[++j] = '.'; s[++j] = '*';
      }
      s[++j] = '\0';
      if ((result = suggest(s)) == nil) return 0;
    }
    const char* r = [result UTF8String];
    format_and_print(r, (int)strlen(r), word, arglen);
    return 0;
  }

  int printed = 0;
  for (NSString* result in results) {
    const char* r = [result UTF8String];
    int len = (int)strlen(r);
    if (len < 1) continue;
    if (printed > 0) printf("\n--------------------\n");
    format_and_print(r, len, word, arglen);
    printed++;
  }
  return 0;
}
