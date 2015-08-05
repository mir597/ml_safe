/*******************************************************************************
    Copyright (c) 2012-2014, KAIST, S-Core.
    All rights reserved.

    Use is subject to license terms.

    This distribution may include materials developed by third parties.
 ******************************************************************************/

package kr.ac.kaist.jsaf;

import java.util.ArrayList;

public class ShellParameters
{
    ////////////////////////////////////////////////////////////////////////////////
    // Command Enumeration
    ////////////////////////////////////////////////////////////////////////////////
    public static final int                        CMD_USAGE = 0;
    public static final int                        CMD_PARSE = 1;
    public static final int                        CMD_UNPARSE = 2;
    public static final int                        CMD_CLONE_DETECTOR = 3;
    public static final int                        CMD_URL = 6;
    public static final int                        CMD_WITH = 7;
    public static final int                        CMD_JUNIT = 9;
    public static final int                        CMD_DISAMBIGUATE = 10;
    public static final int                        CMD_COMPILE = 11;
    public static final int                        CMD_ANALYZE = 14;
    public static final int                        CMD_HELP = 99;

    ////////////////////////////////////////////////////////////////////////////////
    // Parameters
    ////////////////////////////////////////////////////////////////////////////////
    public int                                     command;
    public String                                  opt_OutFileName;
    public String                                  opt_ResultFileName;
    public String                                  opt_WALAFileName;
    public String                                  opt_PrettyFileName;
    public String                                  opt_Dir;
    public String                                  opt_HTML;
    public boolean                                 opt_Time;
    public boolean                                 opt_IgnoreErrorOnAST;
    public boolean                                 opt_Model;
    public boolean                                 opt_Mozilla;
    public boolean                                 opt_Verbose1;
    public boolean                                 opt_Verbose2;
    public boolean                                 opt_Verbose3;
    public boolean                                 opt_LocClone;
    public boolean                                 opt_Pretty;
    public boolean                                 opt_TryCatch;
    public boolean                                 opt_Test;
    public boolean                                 opt_DeveloperMode;
    public boolean                                 opt_ErrorOnly;
    public boolean                                 opt_Library;
    public boolean                                 opt_MemDump;
    public boolean                                 opt_ExitDump;
    public boolean                                 opt_StatDump;
    public boolean                                 opt_BottomDump;
    public boolean                                 opt_ScriptDump;
    public boolean                                 opt_Visual;
    public boolean                                 opt_CheckResult;
    public boolean                                 opt_NoAssert;
    public boolean                                 opt_Compare;
    public boolean                                 opt_noStop;
    public boolean                                 opt_skipExternal;
    public boolean                                 opt_XML;
    public boolean                                 opt_Function;

    public int                                     opt_Timeout;
    public int                                     opt_MaxStrSetSize;
    public int                                     opt_MaxLocCount;
    public boolean                                 opt_FunctionCoverage;
    public boolean                                 opt_debugger;
    public boolean                                 opt_debug;
    public String                                  opt_DDGFileName;
    public String                                  opt_DDG0FileName;
    public String                                  opt_FGFileName;
    public String[]                                FileNames;
    public String                                  url;

    ////////////////////////////////////////////////////////////////////////////////
    // Constructor and Initialize
    ////////////////////////////////////////////////////////////////////////////////
    private String                                 ErrorMessage;

    public ShellParameters()
    {
        ErrorMessage = null;
        Clear();
    }

    public void Clear()
    {
        command = CMD_USAGE;
        opt_OutFileName = null;
        opt_ResultFileName = null;
        opt_WALAFileName = null;
        opt_Dir = null;
        opt_HTML = null;
        opt_Time = false;
        opt_IgnoreErrorOnAST = false;
        opt_Model = false;
        opt_Mozilla = false;
        opt_Verbose1 = false;
        opt_Verbose2 = false;
        opt_Verbose3 = false;
        opt_LocClone = false;
        opt_Pretty = false;
        opt_TryCatch = false;
        opt_Test = false;
        opt_DeveloperMode = false;
        opt_ErrorOnly = false;
        opt_Library = false;
        opt_MemDump = false;
        opt_ExitDump = false;
        opt_StatDump = false;
        opt_BottomDump = false;
        opt_ScriptDump = false;
        opt_Visual = false;
        opt_CheckResult = false;
        opt_NoAssert = false;
        opt_Compare = false;
        opt_noStop = false;
        opt_skipExternal = false;
        opt_XML = false;
        opt_Function = false;
        opt_Timeout = 0;
        opt_MaxStrSetSize = 1;
        opt_MaxLocCount = 0;
        opt_FunctionCoverage = false;
        opt_debugger = false;
        opt_DDGFileName = null;
        opt_DDG0FileName = null;
        opt_FGFileName = null;
        FileNames = new String[0];
        url = "";
    }

    /**
     * @param args Parameter tokens.
     * @return Error message if there is an error.
     *         null otherwise.
     */
    public String Set(String[] args)
    {
        ErrorMessage = null;
        Clear();
        Parse(args);
        return ErrorMessage;
    }

    ////////////////////////////////////////////////////////////////////////////////
    // Parsing
    ////////////////////////////////////////////////////////////////////////////////
    /**
     * @param args Parameter tokens to parse.
     */
    private void Parse(String[] args)
    {
        // There is no parameter.
        if(args.length == 0) return;

        // Set feasible options for each command.
        ArrayList<String> feasibleOptions = new ArrayList<String>();

        // For all commands
        feasibleOptions.add("-module");
        feasibleOptions.add("-ignoreErrorOnAST");

        // For each command
        String cmd = args[0];
        if(cmd.compareTo("parse") == 0)
        {
            command = CMD_PARSE;
            feasibleOptions.add("-out");
            feasibleOptions.add("-time");
        }
        else if(cmd.compareTo("unparse") == 0)
        {
            command = CMD_UNPARSE;
            feasibleOptions.add("-out");
        }
        else if(cmd.compareTo("with") == 0)
        {
            command = CMD_WITH;
            feasibleOptions.add("-out");
        }
        else if(cmd.compareTo("junit") == 0)
        {
            command = CMD_JUNIT;
        }
        else if(cmd.compareTo("disambiguate") == 0)
        {
            command = CMD_DISAMBIGUATE;
            feasibleOptions.add("-out");
        }
        else if(cmd.compareTo("compile") == 0)
        {
            command = CMD_COMPILE;
            feasibleOptions.add("-out");
            feasibleOptions.add("-time");
        }
        else if(cmd.compareTo("analyze") == 0)
        {
            if(cmd.compareTo("analyze") == 0) command = CMD_ANALYZE;
            feasibleOptions.add("-out");
            feasibleOptions.add("-result");
            feasibleOptions.add("-wala");
            feasibleOptions.add("-debug");
        }
        else if(cmd.compareTo("help") == 0)
        {
            command = CMD_HELP;
        }
        else
        {
            command = CMD_USAGE;
            return;
        }

        // Extract option parameters.
        for(int i = 1; i < args.length; i++)
        {
            // Is this an option parameter?
            if(args[i].charAt(0) == '-')
            {
                // Is this a feasible parameter for this command?
                if(!feasibleOptions.contains(args[i])) ErrorMessage = (args[i] + " is not a valid flag for `jsaf " + cmd + "`");
                // Set the option.
                else i+= SetOption(args, i);

                // Is there an error?
                if(ErrorMessage != null) break;
            }
            else
            {
                // Copy the rest parameters to input filenames.
                int Rest = args.length - i;
                FileNames = new String[Rest];
                for(int j = 0; j < Rest;j ++) FileNames[j] = args[i + j];
                break;
            }
        }

        if(ErrorMessage != null) Clear();
    }

    private int SetOption(String[] args, int index)
    {
        int ConsumedParameterCount = 0;

        String opt = args[index];
        if(opt.compareTo("-out") == 0 ||
           opt.compareTo("-result") == 0 ||
                opt.compareTo("-wala") == 0)
        {
            if(index + 1 >= args.length)
            {
                ErrorMessage = "`" + opt + "` parameter needs an output filename. See help.";
            } else {
                if(opt.compareTo("-out") == 0) opt_OutFileName = args[index + 1];
                else if(opt.compareTo("-result") == 0) {opt_ResultFileName = args[index + 1];}
                else if(opt.compareTo("-wala") == 0) {opt_WALAFileName = args[index + 1];}
                ConsumedParameterCount = 1;
            }
        }
        else if(opt.compareTo("-dir") == 0)
        {
            if(index + 1 >= args.length)
            {
                ErrorMessage = "`" + opt + "` parameter needs an output filename. See help.";
            } else {
                opt_Dir = args[index + 1];
                ConsumedParameterCount = 1;
            }
        }
        else if(opt.compareTo("-debug") == 0) opt_debug = true;
        else if(opt.compareTo("-time") == 0) opt_Time = true;
        else if(opt.compareTo("-ignoreErrorOnAST") == 0) opt_IgnoreErrorOnAST = true;
        else if(opt.compareTo("-verbose1") == 0) opt_Verbose1 = true;
        else if(opt.compareTo("-verbose2") == 0) opt_Verbose2 = true;
        else if(opt.compareTo("-verbose3") == 0) opt_Verbose3 = true;
        else if(opt.compareTo("-trycatch") == 0) opt_TryCatch = true;
        else if(opt.compareTo("-dev") == 0) opt_DeveloperMode = true;
        else if(opt.compareTo("-erroronly") == 0) opt_ErrorOnly = true;
        else if(opt.compareTo("-test") == 0) opt_Test = true;
        else if(opt.compareTo("-library") == 0) opt_Library = true;
        else if(opt.compareTo("-memdump") == 0) opt_MemDump = true;
        else if(opt.compareTo("-exitdump") == 0) opt_ExitDump = true;
        else if(opt.compareTo("-statdump") == 0) opt_StatDump = true;
        else if(opt.compareTo("-bottomdump") == 0) opt_BottomDump = true;
        else if(opt.compareTo("-scriptdump") == 0) opt_ScriptDump = true;
        else if(opt.compareTo("-visual") == 0) opt_Visual = true;
        else if(opt.compareTo("-checkResult") == 0) opt_CheckResult = true;
        else if(opt.compareTo("-no-assert") == 0) opt_NoAssert = true;
        else if(opt.compareTo("-compare") == 0) opt_Compare = true;
        else if(opt.compareTo("-nostop") == 0) opt_noStop = true;
        else if(opt.compareTo("-skipexternal") == 0) opt_skipExternal = true;
        else if(opt.compareTo("-fcov") == 0) opt_FunctionCoverage = true;
        else if(opt.compareTo("-console") == 0) opt_debugger = true;
        else if(opt.compareTo("-xml") == 0) opt_XML = true;
        else if(opt.compareTo("-function") == 0) opt_Function = true;
        else
        {
            ErrorMessage = "`" + opt + "` is no match for option parameter.";
        }

        return ConsumedParameterCount;
    }
}
