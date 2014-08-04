#!/bin/sh
s="\t "
S="[$s]*"
w="_a-zA-Z0-9"
CN="[A-Z][$w]*"
NM="[$w][$w]*"

SDK="/Applications/Xcode.app/Contents/Developer/Platforms/iPhoneOS.platform/Developer/SDKs/iPhoneOS5.1.sdk/System/Library/Frameworks"

find $SDK -name "*.h" | xargs etags -a --declarations -r "/$S[-+]$S(\($S$NM\)\{1,3\}$S\**$S)?$S\($NM\)$S[:;]/\2/" -f frm.tags
sed "/^@class/d" frm.tags > objc.TAGS