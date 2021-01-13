class Foo:
    def __init__(self, value):
        self.value_ = value

    def add(self, other):
        return Foo(self.value_ + other.value_)

    def value(self):
        return self.value_


def do_add(left, right):
    return left.add(right)

if __name__ == "__main__":
    left = Foo(3)
    right = Foo(4)
    result = do_add(left, right)
    print(result.value())
