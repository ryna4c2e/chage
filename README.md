# CHAGE
A simple language designed for [OSECPU-VM](http://osecpu.osask.jp/wiki/).

# 謝辞
このプロジェクトのサンプルプログラムとして、[OSECPU-VM](http://osecpu.osask.jp/wiki/)に含まれるアプリケーションを一部参考にして、
chageで書き直したものを含んでいます。

# ビルドと実行
事前に、[Haskell Platform](https://www.haskell.org/platform/)と、[OSECPU-VM](http://osecpu.osask.jp/wiki/)の最新版を用意してください。

1. `cabal build`で，`dist/build/chage/chage` に実行ファイルができます。
1. `chage input.chage` を実行すると，`out.b32`という[b32形式](http://osecpu.osask.jp/wiki/?page0097) のOSECPU-VMバイナリが生成されます．

# 文法
## 基本
一つのファイルに，上から順番に文を書くと実行されます．
拡張子は.chageでお願いします．
サンプルコードを示すので，悟って下さい．
あと，文法はだいたい[Rust](http://www.rust-lang.org)のパクリです．だいたいみんな同じようなことを考えるということで...
大体の文法は，サンプルコードを用意したので以下を参照ください．
あと，仕様はコロコロ変わります．

## サンプルコード
```rust
// コメントはこんな感じで書けます．
// 変数宣言です．1行ずつ書いて下さい．
var i: int = 0;
var x: int = 0;
var y: int = 0;
var c: int = 0;

// 代入はこんな感じで．
c = 0;

// 整数の配列はこんな形式にしました．
data hoge = [1, 4, 3, 0, 5, 2, 6, 7];

// api呼び出しです．他にもいくつか種類があります．
api_openWin(0, 0, 256, 256);

// ループは不格好ですがこう書いて下さい．
y = 0;
while y < 256 {
  x = 0;

  while x < 256 {

// if文に丸かっこ（）はいりません．
    if x < y {
      c = x * y;

// else節を省略できるようになりました．
      if c >= 32768 {
        c = c + -32768;
      }

      api_drawPoint(3, c, x, y);
    } else {
// i = (x ^ y) & 7とはまだ書けません．一つの文に一つの演算子のみ書けます．
      i = x ^ y;
      i = i & 7;

//      debug; // デバッガを起動して止めることもできます．

// 配列へのアクセスです．
      c = $hoge[i];
      api_drawPoint(0, c, x, y);
    }

    x = x + 1;
  }

  y = y + 1;
}
```
