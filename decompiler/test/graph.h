#include <cxxtest/TestSuite.h>
  
class GraphTestSuite : public CxxTest::TestSuite {

public:

	void testAddition() {
		TS_ASSERT(1 + 1 > 1);
		TS_ASSERT_EQUALS(1 + 1, 2);
	}
};
