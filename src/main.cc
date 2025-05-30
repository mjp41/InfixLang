// Copyright Microsoft and Project Verona Contributors.
// SPDX-License-Identifier: MIT
#include "infix.h"
#include "trieste/driver.h"

int main(int argc, char **argv) {
  trieste::Driver d("infix", nullptr, infix::parser(), infix::passes());
  return d.run(argc, argv);
}
