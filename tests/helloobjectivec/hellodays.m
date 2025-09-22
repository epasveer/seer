#import <Foundation/Foundation.h>

int main(void) {

    @autoreleasepool {

        NSString* name = @"test string!!!";
        float     myval = 3.75;
        NSArray*  weekArray;

        weekArray = [NSArray arrayWithObjects: @"Sun", @"Mon" @"Tue", @"Wed", @"Thr", @"Fri", @"Sat", nil];

        NSLog(@"name      =  %@", name);
        NSLog(@"myval     =  %f", myval);
        NSLog(@"weekArray =  %@", weekArray);
    }

    return 0;
}

