%{
    #include <algorithm>
    #include <cstddef>
    #include <iostream>
    #include <vector>

    std::vector<long long> parsed_list;

    int yylex();
    void prompt();
    void process_list();
    void print_list(std::vector<long long> const& list);
    void yyerror(char const* message);
%}

%union {
    long long ll_val;
}

%token OPEN_BRACE CLOSE_BRACE COMMA NEWLINE
%token <ll_val> NUMBER

%%

line: list NEWLINE { YYACCEPT; } ;
line: error NEWLINE { YYABORT; } ;
list: empty_list ;
list: nonempty_list ;
empty_list: OPEN_BRACE CLOSE_BRACE ;
nonempty_list: OPEN_BRACE number_list CLOSE_BRACE ;
number_list: NUMBER { parsed_list.push_back($1); };
number_list: number_list COMMA NUMBER { parsed_list.push_back($3); };

%%

int main() {
    while (true) {
        prompt();
        if (yyparse() == 0) {
            process_list();
        }
        parsed_list.clear();
    }
}

void prompt() {
    std::cout << "> ";
}

void process_list() {
    std::cout << "Parsed list: ";
    print_list(parsed_list);
    std::cout << std::endl;

    std::sort(parsed_list.begin(), parsed_list.end());

    std::cout << "Sorted list: ";
    print_list(parsed_list);
    std::cout << std::endl;
}

void print_list(std::vector<long long> const& list) {
    std::cout << '[';
    for (std::size_t i = 0; i < list.size(); ++i) {
        std::cout << list.at(i);
        if (i + 1 != list.size()) {
            std::cout << ", ";
        }
    }
    std::cout << ']';
}

void yyerror(char const* message) {
    std::cout << "Syntax error" << std::endl;
}
