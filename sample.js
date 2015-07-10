function f() {
}

function g() {
  f();
}

function h() {
f();
g();
}

i = function j() {
}
j = function () {
}
h();

