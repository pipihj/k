package org.kframework.kompile;

import org.apache.commons.cli.*;
import org.kframework.utils.ActualPosixParser;

public class KompileOptionsParser {
	Options options;
	HelpFormatter help;

	public KompileOptionsParser() {
		options = new Options();
		help = new HelpFormatter();

		// main options
		OptionGroup main = new OptionGroup();
		Option def = new Option("def", "definition", true, "main file to kompile");
		Option step = new Option("step", "step", true, "name of the compilation phase after which compilation should stop.");

		// verbose and help
		OptionGroup verb = new OptionGroup();
		Option help = new Option("h", "help", false, "prints this message and exits");
		Option version = new Option("version", false, "prints version number");
		Option verbose = new Option("v", "verbose", false, "verbose mode");
		// Option lint = new Option("lint", "lint", false, "Checks your definition for possible logical errors");
		verb.addOption(help);
		verb.addOption(version);
		verb.addOption(verbose);
		// verb.addOption(lint);

		// Option tempDisamb = new Option("tempDisamb", "tempDisamb", false, "temporary to test the java disambiguator");

		// verbose and help
		OptionGroup nofile = new OptionGroup();
		Option nofileopt = new Option("nofile", "nofilename", false, "don't include the long filenames in the XML.");
		nofile.addOption(nofileopt);

		// latex
		OptionGroup tex = new OptionGroup();
		Option pdf = new Option("pdf", false, "generate pdf from definition");
		Option latex = new Option("latex", false, "generate latex from definition");
		Option style = new Option("style", true, "pass styling information to the k.sty package");
		Option maudify = new Option("m", "maudify", false, "maudify the definition");
		Option compile = new Option("c", "compile", false, "compile the definition");
		Option toXml = new Option("xml", false, "generate xml from definition");
		Option toDoc = new Option("doc", "documentation", false, "generate the HTML documentation");

		tex.addOption(latex);
		tex.addOption(pdf);
		tex.addOption(compile);
		tex.addOption(maudify);
		tex.addOption(toXml);
		tex.addOption(toDoc);

		OptionGroup warn = new OptionGroup();
		Option warnings = new Option("w", "warnings", true, "Use with all/none to control warnings display");
		warn.addOption(warnings);

		// language module name
		OptionGroup langGroup = new OptionGroup();
		Option lang = new Option("l", "lang", true, "start module");
		langGroup.addOption(lang);

		// language module name
		OptionGroup langSynGroup = new OptionGroup();
		Option langSyn = new Option("synmod", "syntax-module", true, "start module for syntax");
		langSynGroup.addOption(langSyn);

		// language module name
		// OptionGroup preludeGroup = new OptionGroup();
		// Option prelude = new Option("prelude", true, "Load a different prelude file.");
		// preludeGroup.addOption(prelude);

		// lib
		OptionGroup libGroup = new OptionGroup();
		Option lib = new Option("lib", true, "Specify extra-libraries for compile/runtime.");
		libGroup.addOption(lib);

		Option toHTML = new Option("html", false, "generate html from definition");

		Option kexp = new Option("kexp", false, "retrieve the KExp associated to a definition");

		Option unparse = new Option("unparse", false, "unparse a definition");

		Option addTopCell = new Option("addTopCell", false, "add a top cell to configuration and all rules");

		Option kCells = new Option("k", "kcells", true, "cells which contain komputations");

		Option sortCells = new Option("sortCells", false,
				"sort cells according to the order in the configuration");

		// transition
		Option transition = new Option("transition", true, "<arg> tags to become rewrite rules");
		Option superheat = new Option("superheat", true, "syntax <arg> tags triggering super heating nondetermistic choice for strictness");
		Option supercool = new Option("supercool", true, "rule <arg> tags triggering super cooling tags are space-separated and can include the tag default");

		Option outputFile = new Option("o", "output", true, "specify output file/directory. Default <file>-compiled");

		// matching logic and symbolic execution options
		OptionGroup sym = new OptionGroup();
		Option ml = new Option("ml", "matching-logic", false, "generate support for matching logic prover");
		Option smt = new Option("smt", "generate translation to SMTLib2");
		Option symEq = new Option("symeq", "symbolic-equality", false, "generate sort equalities");
		Option symbolic = new Option("symbolic", false, "generate symbolic semantics");
		sym.addOption(ml);
		sym.addOption(smt);
		sym.addOption(symEq);
		sym.addOption(symbolic);

		// no smt calls generated by kompiler
		OptionGroup symOpt = new OptionGroup();
		Option noSmt = new Option("nosmt", false, "do not call the smt solver; this must be used together with --symbolic");
		symOpt.addOption(noSmt);
		
		// check reachability formulas
		OptionGroup checkOpt = new OptionGroup();
		Option check = new Option("check", true, "checks the set of reachability rules from <arg>");
		checkOpt.addOption(check);
		
		// add options
		options.addOption(new Option("fastKast", false, "Option to test new class instantiation"));
		options.addOptionGroup(verb);
		options.addOptionGroup(main);
		options.addOptionGroup(langGroup);
		options.addOptionGroup(langSynGroup);
		options.addOptionGroup(tex);
		options.addOptionGroup(warn);
		options.addOptionGroup(libGroup);
		options.addOptionGroup(nofile);
		options.addOptionGroup(sym);
		options.addOptionGroup(symOpt);
		options.addOptionGroup(checkOpt);
		// options.addOption(tempDisamb);

		Option loud = new Option("loud", false, "prints an OK message at the end if all is ok.");

		options.addOption(loud);
		options.addOption(toHTML);
		options.addOption(kexp);
		options.addOption(unparse);
		options.addOption(addTopCell);
		options.addOption(kCells);
		options.addOption(sortCells);
		options.addOption(transition);
		options.addOption(supercool);
		options.addOption(superheat);
		options.addOption(def);
		options.addOption(step);
		options.addOption(outputFile);
		options.addOption(style);
	}

	public CommandLine parse(String[] cmd) {
		CommandLineParser parser = new ActualPosixParser();
		try {
			CommandLine cl = parser.parse(options, cmd);
			return cl;
		} catch (ParseException e) {
			org.kframework.utils.Error.silentReport(e.getLocalizedMessage());
			// e.printStackTrace();
		}

		org.kframework.utils.Error.helpExit(help, options);
		return null;
	}

	public Options getOptions() {
		return options;
	}

	public HelpFormatter getHelp() {
		return help;
	}
}
