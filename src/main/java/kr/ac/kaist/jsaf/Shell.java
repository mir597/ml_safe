/*******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf;

import edu.rice.cs.plt.iter.IterUtil;
import edu.rice.cs.plt.tuple.Option;
import kr.ac.kaist.jsaf.compiler.*;
import kr.ac.kaist.jsaf.exceptions.*;
import kr.ac.kaist.jsaf.nodes.IRRoot;
import kr.ac.kaist.jsaf.nodes.Program;
import kr.ac.kaist.jsaf.nodes_util.IRFactory;
import kr.ac.kaist.jsaf.nodes_util.JSAstToConcrete;
import kr.ac.kaist.jsaf.nodes_util.JSFromHTML;
import kr.ac.kaist.jsaf.nodes_util.NodeUtil;
import kr.ac.kaist.jsaf.shell.*;
import kr.ac.kaist.jsaf.useful.Pair;
import kr.ac.kaist.jsaf.useful.Triple;
import kr.ac.kaist.jsaf.useful.Useful;

import java.io.BufferedWriter;
import java.io.FileNotFoundException;
import java.io.FileWriter;
import java.io.IOException;
import java.util.LinkedList;
import java.util.List;

public final class Shell {
    ////////////////////////////////////////////////////////////////////////////////
    // Settings and Environment variables
    ////////////////////////////////////////////////////////////////////////////////
    public static boolean                       debug = false;

    public static ShellParameters               params = new ShellParameters();
    public static boolean                       opt_DisambiguateOnly = false;
    public static String                        printTimeTitle = null;
    private static long                         startTime;

    ////////////////////////////////////////////////////////////////////////////////
    // Main Entry point
    ////////////////////////////////////////////////////////////////////////////////
    /**
     * Main entry point for the jsaf shell.
     * In order to support accurate testing of error messages, this method immediately
     * forwards to its two parameter helper method.
     * *** Please do not directly add code to this method, as it will interfere with testing.
     * *** Tests will silently fail.
     * *** Instead, add code to its helper method.
     */
    public static void main(String[] tokens) throws Throwable {
        // Call the internal main function
        main(false, tokens);
    }

    /**
     * Helper method that allows main to be called from tests
     * (without having to worry about System.exit).
     */
    public static void main(boolean runFromTests, String[] tokens) throws Throwable {
        int return_code = -1;

        // If there is no parameter then just print a usage message.
        if(tokens.length == 0) printUsageMessage();
        else return_code = subMain(tokens);

        // If there is an error and this main function is not called by the test
        //   then call the System.exit function to return the error code.
        if(return_code != 0 && !runFromTests) System.exit(return_code);
    }

    public static int subMain(String[] tokens) throws Throwable {
        // Now match the assembled string.
        int return_code = 0;
        try {
            // Parse parameters
            String errorMessage = params.Set(tokens);
            if(errorMessage != null) throw new UserError(errorMessage);

            // Set the start time.
            startTime = System.currentTimeMillis();

            switch(params.command) {
            default :
            case ShellParameters.CMD_USAGE :
                printUsageMessage();
                break;
            case ShellParameters.CMD_PARSE :
                return_code = ParseMain.parse();
                break;
            case ShellParameters.CMD_UNPARSE :
                return_code = UnparseMain.unparse();
                break;
            case ShellParameters.CMD_URL :
                return_code = URLMain.url();
                break;
            case ShellParameters.CMD_WITH :
                return_code = WithMain.withRewriter();
                break;
            case ShellParameters.CMD_JUNIT :
                return_code = JUnitMain.junit();
                break;
            case ShellParameters.CMD_DISAMBIGUATE :
                opt_DisambiguateOnly = true;
                return_code = CompileMain.compile();
                break;
            case ShellParameters.CMD_COMPILE :
                CompileMain.compile();
                break;
            case ShellParameters.CMD_ANALYZE :
                return_code = AnalyzeMain.analyze();
                break;
            case ShellParameters.CMD_IDENTIFIER:
                return_code = IdentifierMain.analyze();
                break;
            case ShellParameters.CMD_HELP :
                printHelpMessage();
                break;
            }
        } catch (ParserError e) {
            System.err.println(e);
            return_code = -1;
        } catch (StaticError e) {
            System.err.println(e);
            return_code = -1;
        } catch (UserError e) {
            System.err.println(e);
            return_code = -1;
        } /*catch (IOException error) {
            System.err.println(error.getMessage());
            return_code = -2;
        }*/

        // Print elapsed time.
        if(printTimeTitle != null)
            System.out.println(printTimeTitle + " took " + (System.currentTimeMillis() - startTime) + "ms.");

        return return_code;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Usage and Help messages
    ////////////////////////////////////////////////////////////////////////////////
    /**
     * Helper method to print usage message.
     */
    private static void printUsageMessage() {
        System.err.print(
            "Usage:\n" +
            " parse [-out file] [-time] somefile.js ...\n" +
            " unparse [-out file] somefile.tjs\n" +
            " url [-out file] someurl\n" +
            " with [-out file] somefile.js ...\n" +
            " module [-out file] somefile.js ...\n" +
            " junit sometest.test ...\n" +
            " disambiguate [-out file] somefile.js ...\n" +
            " compile [-out file] [-time] somefile.js ...\n" +
            "\n" +
            " help\n"
        );
    }

    /**
     * Helper method to print help message.
     */
    private static void printHelpMessage() {
        System.err.print
        ("Invoked as script: jsf args\n"+
         "Invoked by java: java ... kr.ac.kaist.jsaf.Shell args\n"+
         "jsaf parse [-out file] [-time] somefile.js ...\n"+
         "  Parses files. If parsing succeeds the message \"Ok\" will be printed.\n"+
         "  The files are concatenated in the given order before being parsed.\n"+
         "  If -out file is given, the parsed AST will be written to the file.\n"+
         "  If -time is given, the time it takes will be printed.\n"+
         "\n"+
         "jsaf unparse [-out file] somefile.tjs\n"+
         "  Converts a parsed file back to JavaScript source code. The output will be dumped to stdout if -out is not given.\n"+
         "  If -out file is given, the unparsed source code will be written to the file.\n"+
         "\n"+
         "jsaf url [-out file] someurl\n"+
         "  Extracts JavaScript source code from a url and writes it to a file, if any.\n"+
         "  If -out file is given, the extracted source code will be written to the file.\n"+
         "\n"+
         "jsaf with [-out file] somefile.js ...\n"+
         "  Rewrites JavaScript source codes using the with statement to another one without using the with statement.\n"+
         "  If it succeeds the message \"Ok\" will be printed.\n"+
         "  The files are concatenated in the given order before being parsed.\n"+
         "  If -out file is given, the rewritten source code will be written to the file.\n"+
         "\n"+
         "jsaf module [-out file] somefile.js ...\n"+
         "  Rewrites JavaScript source codes using the module syntax to another one without using the module syntax.\n"+
         "  The files are concatenated in the given order before being parsed.\n"+
         "  If -out file is given, the rewritten source code will be written to the file.\n"+
         "\n"+
         "jsaf junit somefile1.test ...\n"+
         "  Runs the system test file(s) somefile1.test (etc) in a junit textui harness.\n"+
         "\n"+
         "jsaf disambiguate [-out file] somefile.js ...\n"+
         "  Disambiguates references in JavaScript source files.\n"+
         "  The files are concatenated in the given order before being parsed.\n"+
         "  If -out file is given, the disambiguated AST will be written to the file.\n"+
         "\n"+
         "jsaf compile [-out file] [-time] somefile.js ...\n"+
         "  Translates JavaScript source files to IR.\n"+
         "  If the compilation succeeds the message \"Ok\" will be printed.\n"+
         "  The files are concatenated in the given order before being parsed.\n"+
         "  If -out file is given, the resulting IR will be written to the file.\n"+
         "  If -time is given, the time it takes will be printed.\n"
        );
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Compile to IR
    ////////////////////////////////////////////////////////////////////////////////
    // Triple<String, Integer, String> : filename, starting line number, JavaScript source
    public static Option<IRRoot> scriptToIR(List<Triple<String, Integer, String>> scripts, Option<String> out) throws UserError, IOException {
        Program program = Parser.scriptToAST(scripts);
        return ASTtoIR(scripts.get(0).first(), program, out).first();
    }

    public static Option<IRRoot> fileToIR(List<String> files, Option<String> out) throws UserError, IOException {
        Program program;
        // html file support 
        if(files.size() == 1 && (files.get(0).toLowerCase().endsWith(".html") || files.get(0).toLowerCase().endsWith(".xhtml") || files.get(0).toLowerCase().endsWith(".htm"))) { 
            // DOM mode
            JSFromHTML jshtml = new JSFromHTML(files.get(0));
            // Parse JavaScript code in the target html file
            program = jshtml.parseScripts();
        } else program = Parser.fileToAST(files);

        // Program program = Parser.fileToAST(files);
        return ASTtoIR(files.get(0), program, out).first();
    }

    public static Pair<Option<IRRoot>, Program> ASTtoIR(String file, Program pgm, Option<String> out) throws UserError, IOException {
        try {
            Program program = pgm;

            // Hoister
            Hoister hoister = new Hoister(program);
            program = (Program)hoister.doit();
            /* Testing Hoister...
            if (out.isSome()){
                String outfile = out.unwrap();
                try{
                    ASTIO.writeJavaAst(program, outfile);
                    System.out.println("Dumped hoisted code to " + outfile);
                } catch (IOException e){
                    throw new IOException("IOException " + e +
                                          "while writing " + outfile);
                }
            }
            */

            // Disambiguator
            Disambiguator disambiguator = new Disambiguator(program, opt_DisambiguateOnly);
            program = (Program)disambiguator.doit();
            List<StaticError> errors = disambiguator.getErrors();

            // Testing Disambiguator...
            if (opt_DisambiguateOnly) {
                if (out.isSome()) {
                    String outfile = out.unwrap();
                    try {
                        Pair<FileWriter, BufferedWriter> pair = Useful.filenameToBufferedWriter(outfile);
                        FileWriter fw = pair.first();
                        BufferedWriter writer = pair.second();
                        writer.write(JSAstToConcrete.doitInternal(program));
                        writer.close();
                        fw.close();
                    } catch (IOException e){
                        throw new IOException("IOException " + e +
                                              "while writing " + outfile);
                    }
                } else if (errors.isEmpty()) {
                    System.out.println(JSAstToConcrete.doit(program));
                }
                reportErrors(NodeUtil.getFileName(program),
                             flattenErrors(errors),
                             Option.<Pair<FileWriter,BufferedWriter>>none());
                if (opt_DisambiguateOnly && errors.isEmpty())
                  return new Pair<Option<IRRoot>, Program>(Option.some(IRFactory.makeRoot()), program);
                return new Pair<Option<IRRoot>, Program>(Option.<IRRoot>none(),
                                                                          program);
            } else {
                WithRewriter withRewriter = new WithRewriter(program, false);
                program = (Program)withRewriter.doit();
                errors.addAll(withRewriter.getErrors());
                Translator translator = new Translator(program);
                IRRoot ir = (IRRoot)translator.doit();
                errors.addAll(translator.getErrors());
                if (errors.isEmpty()) {
                    return new Pair<Option<IRRoot>, Program>(Option.some(ir),
                                                                              program);
                } else {
                    reportErrors(NodeUtil.getFileName(program),
                                 flattenErrors(errors),
                                 Option.<Pair<FileWriter,BufferedWriter>>none());
                    return new Pair<Option<IRRoot>, Program>((params.opt_IgnoreErrorOnAST ? Option.some(ir) : Option.<IRRoot>none()),
                                                                              program);
                }
            }
        } catch (FileNotFoundException f) {
            throw new UserError(file + " not found");
        }
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Error Handling
    ////////////////////////////////////////////////////////////////////////////////
    public static List<? extends StaticError> flattenErrors(Iterable<? extends StaticError> ex) {
        List<StaticError> result = new LinkedList<StaticError>();
        for (StaticError err: ex) {
            result.addAll(flattenErrors(err));
        }
        return result;
    }

    public static List<? extends StaticError> flattenErrors(StaticError ex) {
        List<StaticError> result = new LinkedList<StaticError>();
        if (ex instanceof MultipleStaticError) {
            for (StaticError err : ((MultipleStaticError)ex).toJList())
                result.addAll(flattenErrors(err));
        } else result.add(new WrappedException(ex));
        return result;
    }

    public static int reportErrors(String file_name, List<? extends StaticError> errors,
                                   Option<Pair<FileWriter,BufferedWriter>> pair) throws IOException {
        int return_code = 0;
        if (!IterUtil.isEmpty(errors)) {
            for (StaticError error: IterUtil.sort(errors)) {
                if (pair.isSome()) pair.unwrap().second().write(error.getMessage());
                else System.out.println(error.getMessage());
            }
            String err_string;
            int num_errors = IterUtil.sizeOf(errors);
            if (num_errors == 0) {
                // Unreachable code?
                err_string = "File " + file_name + " compiled successfully.";
            } else {
                err_string = "File " + file_name + " has " + num_errors + " error" +
                    (num_errors == 1 ? "." : "s.");
            }
            if (pair.isSome()) pair.unwrap().second().write(err_string);
            else System.out.println(err_string);
            return_code = -2;
        }
        if (pair.isSome()) {
            pair.unwrap().second().close();
            pair.unwrap().first().close();
        }
        return return_code;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // etc
    ////////////////////////////////////////////////////////////////////////////////
    public static Option<String> toOption(String str) {
        if(str == null) return Option.<String>none();
        else return Option.<String>some(str);
    }
}
