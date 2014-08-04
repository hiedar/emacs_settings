#!/bin/sh

### Usage ###
# $ customBuild.sh 2 1 
# 引数 1. 0　・・・　テストビルド
#        1　・・・　STビルド
#        2  ・・・　商用ビルド
# 引数 2. 1　・・・シミュレータにインストールする。
#　　　   0  ・・・シミュレータにインストールしない。
############################################

# 引数チェック(引数が2つ以外なら、エラー)
if test $# -ne 2;then
    echo '引数の数が不正です。'
    exit 1
fi

# 引数チェック(引数1が0〜2じゃなければ、エラー）
if test $1 -lt 0 -o $1 -gt 2;then
    echo '引数1の範囲が不正です(0~2).'
    exit 1
fi

# 引数チェック（引数2が0〜1じゃなければ、エラー)
if test $2 -lt 0 -o $2 -gt 1;then
    echo '引数2の範囲が不正です(0~1).'
    exit 1
fi

# 引数に応じてアイコンを設定。
CONFIG=Debug
if test $1 -eq 0 -o $1 -eq 1;then
    # ST用のアイコン
    sed -e "s/<string>iconRelease<\/string>/<string>iconST<\/string>/" wifitap-Info.plist | less > wifitap-Info.plist3
    \mv wifitap-Info.plist wifitap-Info.plist.backup
    \mv wifitap-Info.plist3 wifitap-Info.plist

    # RELEASE_FLGの変更。
    if test $1 -eq 0;then
	sed -e "s/#define RELEASE_FLG         [0123456789]/#define RELEASE_FLG         0/" define.h | less > define.h2
	\mv define.h define.h.backup
	\mv define.h2 define.h
    else
	sed -e "s/#define RELEASE_FLG         [0123456789]/#define RELEASE_FLG         1/" define.h | less > define.h2
	\mv define.h define.h.backup
	\mv define.h2 define.h
    fi
else
    # Release用のアイコン
    sed -e "s/<string>iconST<\/string>/<string>iconRelease<\/string>/" wifitap-Info.plist | less > wifitap-Info.plist3
    \mv wifitap-Info.plist wifitap-Info.plist.backup
    \mv wifitap-Info.plist3 wifitap-Info.plist

    # RELEASE_FLGの変更。
    sed -e "s/#define RELEASE_FLG         [0123456789]/#define RELEASE_FLG         2/" define.h | less > define.h2
	\mv define.h define.h.backup
	\mv define.h2 define.h
fi

# 引数2が1であれば、シミュレータにインストール