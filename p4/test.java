class B {
  var c = 13904;
  function test() {
    return 10;
  }
  static function main() {
    var b = new B();
    return b.c;
  }
  var d = 10;
}

class A {
  static function main() {
    return B.main();
  }
}