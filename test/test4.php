<?php

$f1 = "ala ma kota";

function f1() {
    return 5;
}

print "zmienna f1: " . $f1 . "\n";
print "funkcja f1: " . (f1()) . "\n";

class A {
    static $x = 15;

    public function __construct() {
        $this->e = "eee";
        $this->f = "fff";
    }
}

class B {
    static $a = 7;
}

A::$x = f1();
print A::$x . "\n";
# print B::$a->$x . "\n";
$ai = new A;
print "ai->x is: " . $ai->e . "\n";

$v = 56;
function f2() {
    global $v;
    print "variable is: $v\n";
}

f2();

function f3() {
    $v = 57;
    f2();
}

$v = 58;
f3();

$b1 = 9;
$b2 = 5 + $b1++ + ($c1=$b1++);
print "b1=$b1, b2=$b2 c1=$c1\n";

$c2;
print "c2=$c2\n";

# const c1 = 123;


class C {
    static $v1 = 1;

    const v2 = 2;
           //$v3 = 3;

    function v2 () {
        return "hello from v2! (C)";
    }

    public function __construct() {
        define(self::v2, "to jest self::v2");
    }
}

class D extends C {
    function v2 () {
        return "hello from v2! (D)";
    }

}

function f4(C $c) {
    # $d = (D) $c;
    $d = $c;
    echo "This is f4: " . ($d->v2()) . "\n";
}

$c = new C();
$d = new D();

f4($d);

print "    v1 is: " . C::$v1  . "\n";
print "[1] v2 is: " . C::v2   . "\n";
print "[2] v2 is: " . $c->v2()  . "\n";
# print "v2 is: " . $c::v2  . "\n";
//print "v3 is: " . $c->$v3 . "\n";

if (1)
    if (0)
        print "[1] branch 1\n";
    else
        print "[1] branch else (1)\n";

$d1 = new A;
print "d1: " . ($d1->f) . "\n";
$d2 = new A();
print "d2: " . ($d2->f) . "\n";
$d3 = A;
print "d3: " . ($d3->f) . "\n";

?>
