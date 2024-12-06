pub mod analyze;
pub mod rewrite;

use analyze::{AnalysisData, TotalAnalyzer};
use rewrite::Rewriter;

use clap::Parser;
use orca_wasm::Module;

#[derive(Parser, Debug)]
#[command(version, about, long_about = None)]
struct Args {
    // Input File Path
    #[arg(short, long, required = true)]
    input_file_path: String,
    // Output File Path
    #[arg(short, long, required = false)]
    output_file_path: Option<String>,
}

fn main() {
    let args = Args::parse();
    let wasm = wat::parse_file(args.input_file_path).expect("unable to convert");
    let module = Module::parse(&wasm, false).expect("Error parsing");
    let mut analyze: TotalAnalyzer<AnalysisData> = analyze::TotalAnalyzer::init_analysis(module);

    let analysis = &mut analyze.analyses[0];
    analysis.run(&analyze.module);
    let mut rewriter = Rewriter::new(analysis);
    rewriter.rewrite(&mut analyze.module);

    let result = analyze.module.encode();
    let out = wasmprinter::print_bytes(result).expect(":(");
    println!("{}", out);
}

#[test]
fn test_cfg() {
    let path = "test/cfg_test.wat".to_string();
    let wasm = wat::parse_file(path).expect("unable to convert");
    let module = Module::parse(&wasm, false).expect("Error parsing");
    let mut analyzer: TotalAnalyzer<AnalysisData> = analyze::TotalAnalyzer::init_analysis(module);
    analyzer.analyses[0].run(&analyzer.module);
    dbg!(&analyzer.analyses[0].continuations);
}

#[test]
fn test_global_escape() {
    let path = "test/global_escape.wat".to_string();
    let wasm = wat::parse_file(path).expect("unable to convert");
    let module = Module::parse(&wasm, false).expect("Error parsing");
    let mut analyzer: TotalAnalyzer<AnalysisData> = analyze::TotalAnalyzer::init_analysis(module);
    analyzer.analyses[0].run(&analyzer.module);
    dbg!(&analyzer.analyses[0].continuations);
}

#[test]
fn test_simple_alias() {
    let path = "test/simple_alias.wat".to_string();
    let wasm = wat::parse_file(path).expect("unable to convert");
    let module = Module::parse(&wasm, false).expect("Error parsing");
    let mut analyzer: TotalAnalyzer<AnalysisData> = analyze::TotalAnalyzer::init_analysis(module);
    analyzer.analyses[0].run(&analyzer.module);
    dbg!(&analyzer.analyses[0].continuations);
}
