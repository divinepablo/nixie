#include <string>
#include "lexer.hpp"
#include "parser.hpp"
#include <iostream>
#include <fstream>
#include <sstream>
#include <string>
#include "codegen.hpp"
#include <argparse.hpp>

std::string read_file(const std::string& filename) {
    std::ifstream file(filename);
    if (!file.is_open()) {
        throw std::runtime_error("Could not open file: " + filename);
    }

    // Efficiently read the whole file into a string
    std::stringstream buffer;
    buffer << file.rdbuf();
    return buffer.str();
}

int main(int argc, char* argv[]) {
    argparse::ArgumentParser program("nixie", "0.1.0", argparse::default_arguments::all);

    program.add_argument("filename")
        .help("The source file to compile")
        .required();

    program.add_argument("-o", "--output")
        .help("The output filename (default: input file with .o65 extension)")
        .default_value(std::string(""));
    
    program.add_argument("--emit-tokens")
        .help("Emit the tokens to stdout")
        .default_value(false)
        .implicit_value(true);
    
    program.add_argument("--emit-ast")
        .help("Emit the AST to stdout")
        .default_value(false)
        .implicit_value(true);

    program.add_argument("--vector-table", "--vectors", "--vector", "-V")
        .help("Generate 6502 interrupt vector table")
        .default_value(false)
        .implicit_value(true);

    try {
        program.parse_args(argc, argv);
    }
    catch (const std::exception& err) {
        std::cerr << err.what() << std::endl;
        std::cerr << program;
        return 1;
    }

    std::string filename = program.get<std::string>("filename");
    std::string output = program.get<std::string>("--output");
    // bool emit_tokens = program.get<bool>("--emit-tokens");
    // bool emit_ast = program.get<bool>("--emit-ast");
    bool vector_table = program.get<bool>("--vector-table");
    // if (argc < 2) {
    //     std::cerr << "Usage: nixie [filename]\n";
    //     return 1;
    // }

    try {
        std::cout << "Compiling " << filename << "...\n";
        // 1. Read file (This string MUST stay in scope)
        std::string source_code = read_file(filename);

        // 2. Lex (Pass the string_view)
        Lexer lexer(source_code);
        auto tokens = lexer.tokenize();
        // 3. Print tokens
        if (program.get<bool>("--emit-tokens")) {
            for (const auto& token : tokens) {
                std::cout << "Token(Type: " << static_cast<int>(token.type)
                          << ", Value: \"" << token.value << "\")\n";
            }
        }
        Parser parser(tokens);
        auto parsed = parser.parse();

        // 4. Codegen
        CodegenVisitor codegen;
        parsed->accept(codegen);

        codegen.addVectorTable = vector_table;

        // 5. Output binary
        auto binary = codegen.generateO65();
        auto out_filename = output.empty() ? std::string(filename) : output;
        size_t dot_pos = out_filename.find_last_of('.');
        if (dot_pos != std::string::npos) {
            out_filename = out_filename.substr(0, dot_pos);
        }
        out_filename += ".o65";
        std::ofstream outfile(out_filename, std::ios::binary);
        outfile.write(reinterpret_cast<const char*>(binary.data()), binary.size());
        std::cout << "Compilation successful! Output written to " << out_filename << "\n";

    } catch (const std::exception& e) {
        std::cerr << "Error: " << e.what() << "\n";
        return 1;
    }

    return 0;
}