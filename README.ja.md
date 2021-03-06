Prely Functional Data Structures 読書ノート
=================================================

このリポジトリは、Chris Okasaki著

Purely Functional Data Structures
http://www.amazon.co.jp/dp/0521663504

の読書ノートです。本に記述されたサンプルコードに加え、
本文で示されたデータ構造やアルゴリズム、練習問題の実装を行っています。

本が解説に使用する言語はStandard ML (SML)ですが、付録にはHaskell版も掲載されています。
しかし、一部の遅延評価のテクニックを使用したデータ構造やアルゴリズムでは、
正格評価を行う個所と遅延評価を行う個所を厳密に区別する必要があるため、
Haskell版の記載がありません。

そこで、このリポジトリでは、OCamlでの実装をメインに行っています。
当初はHaskellでいくつか実装を書いていましたが、4章と6章以降はOCamlが中心です
(しかし真面目にディレクトリを分割しているのはHaskell版:-)。
OCamlはSMLと同様に基本は正格評価でありながら、Lazyモジュールを使うことで
遅延評価を行うアルゴリズムが記述可能なためです。

ただし、本の中でOkasakiさんがSMLに実装したような、遅延評価の表記を簡易化する
Syntax Sugarの導入などはほとんど行っていないので、記述はかなり冗長です。
そのようななかでひとつだけ、Lazyパターンを使用しています。
そのため実行にはOCaml 3.12以降が必要です。

この本は、PFDS読書会に参加しながら読み進めました。
OCamlについては何も知らなかったので@khibinoさんと@master_qさんが作っていた
PFDSリポジトリのOCaml実装を大変参考にしています。

実行に必要な環境(たぶん):
- OCaml環境
  - OCaml 3.12以降
  - findlib
  - OUnit
- Haskell環境
  - GHC 7.0.1以降

PFDS読書会まとめ
http://wiki.haskell.jp/Workshop/ReadPFDS/

