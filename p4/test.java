class B {
  var c = 10;
  function test() {
    return 10;
  }
  static function main() {
    var b = new B();
    return b.test();
  }
}

class A {
  static function main() {
    return B.main();
  }
}