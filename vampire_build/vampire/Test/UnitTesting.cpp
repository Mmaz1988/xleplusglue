/*
 * This file is part of the source code of the software program
 * Vampire. It is protected by applicable
 * copyright laws.
 *
 * This source code is distributed under the licence found here
 * https://vprover.github.io/license.html
 * and in the source directory
 */
/**
 * @file RuntimeStatistics.cpp
 * Implements class RuntimeStatistics.
 */

#include <iomanip>
#include <fstream>

#include "Lib/Environment.hpp"
#include "Lib/System.hpp"
#include "Lib/Sys/Multiprocessing.hpp"
#include "Shell/Options.hpp"

#include "Lib/Comparison.hpp"
#include "Lib/Int.hpp"
#include "Lib/Sort.hpp"
#include "Lib/Stack.hpp"

#include "UnitTesting.hpp"

namespace Test {

using namespace std;
using namespace Lib;
using namespace Lib::Sys;

TestUnit::TestUnit(vstring const& name)
: _tests(), _name(name)
{ }

UnitTesting* UnitTesting::_instance = nullptr;  

UnitTesting& UnitTesting::instance() 
{ 
  if (_instance == nullptr) {
    _instance = new UnitTesting();
  }
  return *_instance; 
}

bool UnitTesting::runTest(vstring const& unitId, vstring const& testName) 
{
  auto unit = findUnit(unitId);
  if (unit == nullptr) return false;
  else if (unit->hasTest(testName)) {
    return unit->runTest(testName);
  } else {
    return unit->runTestsWithNameSubstring(testName, std::cout);
  }
}

TestUnit::Test* TestUnit::findTest(vstring const& testCase)
{
  for (auto& test : _tests) {
    if (test.name == testCase) {
      return &test;
    }
  }
  return nullptr;
}
bool TestUnit::hasTest(vstring const& name)
{ return findTest(name) != nullptr; }


bool TestUnit::runTest(vstring const& name)
{
  auto test = findTest(name);
  if (test != nullptr) {
      test->proc();
      return true;
  } else {
    std::cerr << "test \"" << name << "\" not found in " << id() << std::endl;
    return false;
  }
}

bool UnitTesting::run(Stack<vstring> const& args) 
{
  if (args.size() == 2) {
    return runTest(args[0], args[1]);
  } else if (args.size() == 1) {
    return runUnit(args[0]);
  } else {
    std::cerr << "usage: vtest <unit-name> [ <test-name-substring> ]";
    exit(-1);
  }
}

TestUnit* UnitTesting::findUnit(vstring const& id) 
{
  TestUnit* found = nullptr;
  for (auto& test : _units) {
    if (test.id() == id) {
      if (found == nullptr) {
        found = &test;
        break;
      } else {
        std::cerr << "found duplicate test id: " << test.id() << std::endl;
        return nullptr;
      }
    }
  }
  if (found == nullptr) {
    std::cerr << "test not found: " << id << std::endl;
  }
  return found;
}

bool UnitTesting::runUnit(vstring const& id)
{
  auto unit = findUnit(id);
  if (unit == nullptr) return false;
  else return unit->run(std::cout);
}

bool UnitTesting::listTests(Stack<vstring> const&)
{
  auto& out = std::cout;
  for (auto unit : _units) {
    for (auto test : unit.tests()) {
      out << unit.id() << "\t" << test.name << std::endl;
    }
  }
  return true;
}

bool TestUnit::runTestsWithNameSubstring(vstring const& pref, ostream& out)
{
  Stack<Test>::BottomFirstIterator uit(_tests);

  if(!uit.hasNext()) {
    out<<"No tests to run."<<endl;
  }
  unsigned cnt_fail = 0;
  unsigned cnt_ok  = 0;
  while(uit.hasNext()) {
    TestUnit::Test t=uit.next();
    if (vstring(t.name).find(pref) != vstring::npos) {
      out << "Running " << t.name << "... \r";
      out.flush();
      bool ok;
      {
        ok = spawnTest(t.proc);
      }
      out << "\r" << ( ok ? "[  OK  ]" : "[ FAIL ]" ) << " " << t.name << "          " << endl;
      if (ok) cnt_ok++;
      else cnt_fail++;
    }
  }
  out << endl;
  auto cnt = cnt_fail + cnt_ok;
  out << fixed << setprecision(1);
  out << "Tests run: " << cnt << endl;
  out << "  - ok   " << cnt_ok   << "\t(" << (cnt_ok   * 100.0 / cnt) << ") %" << endl;
  out << "  - fail " << cnt_fail << "\t(" << (cnt_fail * 100.0 / cnt) << ") %" << endl;
  return cnt_fail == 0;
}

bool TestUnit::run(ostream& out)
{ return runTestsWithNameSubstring("", out); }

void TestUnit::add(Test t)
{ _tests.push(t); }

TestAdder::TestAdder(const char* unitId, TestProc proc, const char* name)
{ UnitTesting::instance().add(unitId, TestUnit::Test(proc, name)); }

/**
 * Run test in a different process and wait for its termination
 * This is to provide isolation when running multiple tests in one go.
 *
 * returns true iff the test process exited with status code 0
 */
bool TestUnit::spawnTest(TestProc proc)
{
  auto mp = Multiprocessing::instance();
  pid_t fres = mp->fork();
  if(fres == 0) {
    try {
      proc();
    } catch (Lib::Exception& e) {
      e.cry(std::cerr);
      exit(-1);
    } catch (std::exception& e) {
      std::cerr << e.what() << std::endl;
      exit(-1);
    }
    exit(0);
  } else {
    int childRes;
    Multiprocessing::instance()->waitForChildTermination(childRes);
    return  childRes == 0;
  }
}

bool UnitTesting::add(vstring const& testUnit, TestUnit::Test test)
{
  for (auto& unit : _units) {
    if (unit.id() == testUnit) {
      unit.add(test);
      return true;
    }
  }
  _units.push(TestUnit(testUnit));
  _units.top().add(test);
  return true;
}

std::ostream& operator<<(ostream& out, TestUnit::Test const& t) 
{ return out << t.name; }

} // namespace Test

int main(int argc, const char** argv) 
{
  using namespace Lib;
  using namespace std;

  // enable tracebacks in failing unit tests by default
  System::registerArgv0(argv[0]);
  env.options->setTraceback(true);

  bool success;
  auto cmd = vstring(argv[1]);
  auto args = Stack<vstring>(argc - 2);
  for (int i = 2; i < argc; i++) {
    args.push(vstring(argv[i]));
  }
  if (cmd == "ls") {
    success = Test::UnitTesting::instance().listTests(args);
  } else if (cmd == "run") {
    success = Test::UnitTesting::instance().run(args);
  } else {
    cerr << "unknown command: " << cmd << endl;
    success = false;
  }
  return success ? 0 : -1;
}

