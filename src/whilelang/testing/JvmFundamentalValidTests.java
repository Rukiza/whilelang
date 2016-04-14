package whilelang.testing;

import java.util.Collection;

import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;


@RunWith(Parameterized.class)
public class JvmFundamentalValidTests extends AbstractJvmValidTests {
	
	/**
	 * The set of tests which are suitable for this part of the assignment
	 */
	private static String[] tests = {
			// Part 1 Tests
			"BoolAssign_Valid_1",
			"BoolAssign_Valid_2",
			"BoolAssign_Valid_3",
			"BoolAssign_Valid_4",
			"BoolIfElse_Valid_1",
			"BoolIfElse_Valid_2",
			"BoolReturn_Valid_1",
			"Comments_Valid_10",
			"Comments_Valid_11",
			"Comments_Valid_12",
			"Comments_Valid_13",
			"Comments_Valid_1",
			"Comments_Valid_2",
			"Comments_Valid_3",
			"Comments_Valid_4",
			"Comments_Valid_5",
			"Comments_Valid_6",
			"Comments_Valid_7",
			"Comments_Valid_8",
			"Comments_Valid_9",
			"IfElse_Valid_1",
			"IfElse_Valid_2",
			"IfElse_Valid_3",
			"IfElse_Valid_4a",
			"IfElse_Valid_4b",
			"IfElse_Valid_4c",
			"IfElse_Valid_4d",
			"IfElse_Valid_4e",
			"IfElse_Valid_4",
			"IntDefine_Valid_1",
			"IntDiv_Valid_1",
			"IntDiv_Valid_2",
			"IntEquals_Valid_1",
			"IntMul_Valid_1",
			"Method_Valid_1",
			"Method_Valid_2",
			"Method_Valid_3",			
			"Remainder_Valid_1",
			"String_Valid_11",
			"String_Valid_13",
			"String_Valid_15",
			"String_Valid_6",
			"String_Valid_7",	
	};
	
	public JvmFundamentalValidTests(String testName) {
		super(testName);
	}

	// Here we enumerate all available test cases.
	@Parameters(name = "{0}")
	public static Collection<Object[]> data() {
		return AbstractJvmValidTests.data(tests);
	}	
}
