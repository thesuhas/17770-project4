pub mod analyze;
use clap::Parser;

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
    println!("Input File: {:?}, Output File: {:?}", args.input_file_path, args.output_file_path);
}
