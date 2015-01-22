// Authors: Charles Francis <agentcoops@gmail.com>
//          Chunyang Xu <xuchunyang56@gmail.com>
// Inspired by: dictionary.m from dictionary.vim (https://github.com/itchyny/dictionary.vim)
// Last Change: Thu Jan 22 21:06:42 2015

#import <Foundation/Foundation.h>
#import <CoreServices/CoreServices.h>

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
Options:\n\
  -h                Show this help message and exit\n\
  -l                Display list of available dictionaries and exit\n\
  -u                Use only special dictionary, otherwise use default dictionary\n");
}

int main(int argc, char *argv[]) {
  char *dict_name = NULL;
  char *word = NULL;
  NSArray *dicts = NULL;
  NSDictionary *s_names = NULL;;
  int c;

  opterr = 0;
  while ((c = getopt(argc, argv, "hlu:")) != -1)
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
      printf("dict=%s, word=%s\n", dict_name, word);
      NSLog(@"%@\n", defStr);
    }
  } else if (word) {            // Use Default Dictionary
    NSString* ns_word = [NSString stringWithUTF8String:word];
    NSLog(@"%@", (NSString*)DCSCopyTextDefinition(NULL, (CFStringRef)ns_word,
                                                  CFRangeMake(0,[ns_word length])));
  } else {
    usage();
    return -1;
  }

  return 0;
}

// Local Variables:
// fill-column: 100
// c-basic-offset: 2
// End:
