package whilelang.testing;

import java.io.File;
import java.io.IOException;
import java.util.ArrayList;
import java.util.Collection;

import static org.junit.Assert.*;
import org.junit.*;
import org.junit.runner.RunWith;
import org.junit.runners.Parameterized;
import org.junit.runners.Parameterized.Parameters;

import whilelang.ast.WhileFile;
import whilelang.compiler.Parser;
import whilelang.compiler.WhileCompiler;
import whilelang.util.Interpreter;

@RunWith(Parameterized.class)
public class CastTests {
private static final String WHILE_SRC_DIR = "tests/casts/".replace('/', File.separatorChar);
	
	private final String testName;
	
	public CastTests(String testName) {
		this.testName = testName;
	}
	
	/**
	 * Run the compiler over the test case. If it's an invalid test, then we
	 * expect it to fail. Otherwise, we expect it to pass.
	 * 
	 * @param filename
	 * @throws IOException
	 */
	private void runTest(String testname) throws IOException {			
		if(testname.contains("Valid")) {
			compileAndRun(testname);
		} else {
			try {
				compileAndRun(testname);
				fail();
			} catch (Exception e) {
				// Success!
			}
		}
	}
	
	private void compileAndRun(String testname) throws IOException {
		File srcFile = new File(WHILE_SRC_DIR + testname + ".while");
		WhileCompiler compiler = new WhileCompiler(srcFile.getPath());
		WhileFile ast = compiler.compile();		
		new Interpreter().run(ast);
	}
			
	// ===============================================================
	// Test Harness
	// ===============================================================
	
	// Here we enumerate all available test cases.
	@Parameters(name = "{0}")
	public static Collection<Object[]> data() {
		ArrayList<Object[]> testcases = new ArrayList<Object[]>();
		for (File f : new File(WHILE_SRC_DIR).listFiles()) {
			if (f.isFile()) {
				String name = f.getName();
				if (name.endsWith(".while")) {
					// Get rid of ".while" extension
					String testName = name.substring(0, name.length() - 6);
					testcases.add(new Object[] { testName });
				}
			}
		}
		return testcases;
	}

	@Test
	public void valid() throws IOException {
		runTest(this.testName);
	}
}
