#include <string>
#include "lexer.hpp"
#include "parser.hpp"
#include "linker.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include <argparse.hpp>

int main(int argc, char* argv[]) {
    argparse::ArgumentParser program("nixie", "0.1.0", argparse::default_arguments::all);

    program.add_argument("files")
        .help("The source file to compile")
        .nargs(argparse::nargs_pattern::at_least_one)
        .required();

    program.add_argument("-o", "--output")
        .help("The output filename (default: input file with .o65 extension)")
        .default_value(std::string(""));

    program.add_argument("--raw", "-r")
        .help("Output raw binary instead of .o65 format")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("--verbose")
        .help("Enable verbose output")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("--fill")
        .help("Fill unused space in output with 0x00 bytes")
        .default_value(false)
        .implicit_value(true);
    
    program.add_argument("-Z")
        .help("Only decide base addresses for segments with base address == 0x0000")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("--text")
        .scan<'i', uint16_t>()
        .help("Set the base address for the .text segment (default: 0x0000)")
        .default_value(static_cast<uint16_t>(0x0000));

    program.add_argument("--data")
        .scan<'i', uint16_t>()
        .help("Set the base address for the .data segment (default: 0x0000)")
        .default_value(static_cast<uint16_t>(0x0000));

    program.add_argument("--bss")
        .scan<'i', uint16_t>()
        .help("Set the base address for the .bss segment (default: 0x0000)")
        .default_value(static_cast<uint16_t>(0x0000));

    program.add_argument("--zero")
        .scan<'i', uint16_t>()
        .help("Set the base address for the .zero segment (default: 0x0000)")
        .default_value(static_cast<uint16_t>(0x0000));

    try {
        program.parse_args(argc, argv);
    }
    catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    std::list<std::string> files = program.get<std::list<std::string>>("files");
    std::string output = program.get<std::string>("--output");
    bool raw_output = true; // program.get<bool>("--raw"); // for now, always output raw binary, .o65 support is not implemented yet
    if (output.empty()) {
        output = files.front() + (raw_output ? ".bin" : ".o65");
    }
    bool fill = program.get<bool>("--fill");
    bool verbose = program.get<bool>("--verbose");
    bool only_decide_zero_bases = program.get<bool>("-Z");
    uint16_t text_base = program.get<uint16_t>("--text");
    uint16_t data_base = program.get<uint16_t>("--data");
    uint16_t bss_base  = program.get<uint16_t>("--bss");
    uint16_t zero_base = program.get<uint16_t>("--zero");
    LinkerConfig config{
        .files = files,
        .output = output,
        .raw_output = raw_output,
        .fill = fill,
        .verbose = verbose,
        .only_decide_zero_bases = only_decide_zero_bases,
        .text_base = text_base,
        .data_base = data_base,
        .bss_base = bss_base,
        .zero_base = zero_base
    };
    try {
        Linker linker(config);
        linker.load_files();
        // std::cout << "Loaded " << files.size() << " object file(s).\n";
        auto hi = linker.link();
        std::ofstream output_file(config.output, std::ios::binary);
        if (!output_file.is_open()) {
            throw std::runtime_error("Could not open output file: " + config.output);
        }
        std::cout << "Writing output to " << config.output << "...\n";
        output_file.write(reinterpret_cast<const char*>(hi.data()), hi.size());
    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}

