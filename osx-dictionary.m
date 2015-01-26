// Filename: osx-dictionary.m
// Program: Access Mac OS X Dictionary.app from Command Line
// Authors: Charles Francis <agentcoops@gmail.com>
//          Chunyang Xu <xuchunyang56@gmail.com>
// Inspired by: dictionary.m from dictionary.vim (https://github.com/itchyny/dictionary.vim)
// Compile: clang -framework CoreServices -framework Foundation osx-dictionary.m -o osx-dictionary-cli
// Usage: osx-dictionary-cli -h
// Last Change: Thu Jan 22 2015

#import <Foundation/Foundation.h>
#import <CoreServices/CoreServices.h>

#include <getopt.h> // getopt_long

// No timestamp and program name
#define NSLog(FORMAT, ...) printf("%s\n", \
                                  [[NSString stringWithFormat:FORMAT, ##__VA_ARGS__] UTF8String]);

// Obtain undocumented dictionary service API calls
// -- thanks to https://github.com/mchinen/RSDeskDict for the research.
CFArrayRef DCSCopyAvailableDictionaries();
CFStringRef DCSDictionaryGetShortName(DCSDictionaryRef);
DCSDictionaryRef DCSDictionaryCreate(CFURLRef url);
CFArrayRef DCSCopyRecordsForSearchString(DCSDictionaryRef dictionary, CFStringRef string,
                                         void *u1, void *u2);
CFStringRef DCSRecordCopyData(CFTypeRef record);
CFStringRef DCSDictionaryGetName(DCSDictionaryRef dictionary);
CFStringRef DCSRecordGetRawHeadword(CFTypeRef record);
CFStringRef DCSRecordGetString(CFTypeRef record);

CFStringRef DCSRecordGetAssociatedObj(CFTypeRef record);
CFStringRef DCSRecordCopyDataURL(CFTypeRef record);
CFStringRef DCSRecordGetAnchor(CFTypeRef record);
CFStringRef DCSRecordGetSubDictionary(CFTypeRef record);
CFStringRef DCSRecordGetTitle(CFTypeRef record);
CFDictionaryRef DCSCopyDefinitionMarkup (DCSDictionaryRef dictionary, CFStringRef record);

extern CFStringRef DCSRecordGetHeadword(CFTypeRef);
extern CFStringRef DCSRecordGetBody(CFTypeRef);

void usage() {
  printf("Usage:\n\
  osx-dictionary-cli: [-h] [-l]\n\
                      [-u dictionary] word\n\
\n\
Mac OS X Dictionary.app Console Version\n\
\n\
positional argument:\n\
  word                        word to lookup\n\
\n\
optional arguments:\n\
  -h, --help                  Show this help message and exit\n\
  -l, --list-dicts            Display list of available dictionaries and exit\n\
  -u, --user-dict=dictionary  Use only special dictionary, otherwise use default dictionary\n");
}

int main(int argc, char *argv[]) {
  char *dict_name = NULL;
  char *word = NULL;
  int word_len = 0;
  NSArray *dicts;
  NSDictionary *s_names;
  NSString *result;

  struct option long_options[] = {
    {"help", no_argument, 0, 'h'},
    {"list-dicts", no_argument, 0, 'l'},
    {"use-dict", required_argument, 0, 'u'},
    {0, 0, 0, 0}
  };
  int option_index = 0;
  int c;
  while ((c = getopt_long(argc, argv, "hlu:", long_options, &option_index)) != -1)
    switch (c) {
    case 'h':
      usage();
      return 0;
    case 'l':
      dicts = (NSArray*)DCSCopyAvailableDictionaries();
      for (NSObject *dict in dicts)
        NSLog(@"%@",
              (__bridge NSString*)DCSDictionaryGetShortName((__bridge DCSDictionaryRef)dict));
      return 0;
    case 'u':
      dict_name = optarg;
      break;
    default:
      usage();
      return -1;
    }

  word = argv[optind];
  if (word == NULL || (word_len = strlen(word)) == 0) {
    usage();
    return -1;
  }

  if (dict_name && word) {
    NSString *dictionaryName = @(dict_name);
    NSString *ns_word = @(word);

    dicts = (NSArray*)DCSCopyAvailableDictionaries();
    s_names = [NSMutableDictionary dictionaryWithCapacity:[dicts count]];
    for (NSObject *d in dicts) {
      NSString *sn = (__bridge NSString*)DCSDictionaryGetShortName((__bridge DCSDictionaryRef)d);
      [s_names setValue:d forKey:sn];
    }

    NSObject *d = [s_names objectForKey:dictionaryName];
    if (d == NULL) {
      fprintf(stderr, "Dictionary '%s' not found\n", dict_name);
      return -1;
    }
    CFRange substringRange = DCSGetTermRangeInString((__bridge DCSDictionaryRef)d,
                                                     (__bridge CFStringRef)ns_word, 0);
    if (substringRange.location == kCFNotFound) {
      fprintf(stderr, "kCFNotFound\n"); // no idea what's this kind of error
      return -1;
    }
    NSString* subStr = [ns_word substringWithRange:NSMakeRange(substringRange.location,
                                                               substringRange.length)];
    NSArray* records = (NSArray*)DCSCopyRecordsForSearchString((__bridge DCSDictionaryRef)d,
                                                               (__bridge CFStringRef)subStr, 0, 0);
    NSString* defStr = @"";
    if (records) {
      for (NSObject* r in records) {
        // CFStringRef data = DCSRecordGetTitle((__bridge CFTypeRef) r);
        CFStringRef data = DCSRecordGetRawHeadword((__bridge CFTypeRef) r);
        // CFStringRef data = DCSRecordGetHeadword((__bridge CFTypeRef) r);
        if (data) {
          NSString* recordDef =
            (NSString*)DCSCopyTextDefinition((__bridge DCSDictionaryRef)d,
                                             data,
                                             CFRangeMake(0,CFStringGetLength(data)));
          defStr = [defStr stringByAppendingString:[NSString stringWithFormat:@"%@\n\n", recordDef]];
        }
      }
    }

    if (defStr == nil || [defStr isEqualToString:@""]) {
      fprintf(stderr, "definition of '%s' not found\n", word);
      return -1;
    } else {
      result = defStr;
    }
  } else if (word) {            // Use Default Dictionary
    NSString* ns_word = [NSString stringWithUTF8String:word];
    result = (NSString*)DCSCopyTextDefinition(NULL, (CFStringRef)ns_word, CFRangeMake(0,[ns_word length]));
    if (result == nil) {
      fprintf(stderr, "definition of '%s' not found\n", word);
      return -1;
    }
  } else {
    usage();
    return -1;
  }

  char* r = (char*)[result UTF8String];
  int len = strlen(r);
  if (len < 1) return 0;
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
  if (word_len + 9 < len) {
    char flg = 1;
    for (i = 0; i < word_len; ++i) {
      if (tolower(r[i]) != tolower(word[i])) {
        flg = 0; break;
      }
    }
    if (flg) {
      if (r[i] == '.' || strncmp(r + i, paren3, 3) == 0) {
        for (i = 0; i < word_len + (r[i] == '.'); ++i)
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
    } else if (i + 3 < len && isdigit(r[i]) && isdigit(r[i + 1]) && r[i + 2] == ' ' && 0 < i && !isdigit(r[i - 1])) {
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
      } else {
        s[j] = r[i];
      }
      slash++;
    } else if (i + 2 < len && isdigit(r[i]) && r[i + 1] == ' ' && 0 < i && !isdigit(r[i - 1])) {
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
    } else if (!num && paren && strncmp(r + i, paren1, 3) == 0 && j > 1 && s[j - 1] != '\n') {
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
    } else if (firstparen == 2 && r[i] == ')') {
      s[j] = r[i];
      if (i + 1 < len && r[i + 1] == ' ') {
        s[++j] = '\n';
        ++i;
      }
      firstparen = 0;
    } else {
      s[j] = r[i];
    }
  }
  s[j] = '\0';
  printf("%s", s);

  return 0;
}

// Local Variables:
// fill-column: 100
// c-basic-offset: 2
// End:
