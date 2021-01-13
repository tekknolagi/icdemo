class Foo {
 public:
  Foo(int value) : value_(value) {}
  Foo add(Foo other);
  int value() { return value_; }

 private:
  int value_;
};
