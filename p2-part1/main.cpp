/*
 * Kevin Bohinski & Evan Melquist
 * 10/9/16
 * CSC 435 Programming Languages
 *
 * Project 2: Part 1
 * Parser Generator
 *
 * main.cpp
 */

#include <iostream>
#include <algorithm>
#include <sstream>
#include <map>
#include <set>
#include <stack>
#include <queue>

#include "./scanner/scan.cc"

using namespace std;

void print_map_string_int(map<string, int> m, string friendly_name);

void print_map_string_vv_string(map<string, vector<vector<string>>> m, string friendly_name);

void print_map_int_vv_int(map<int, vector<vector<int>>> m, string friendly_name);

void print_map_int_s_int(map<int, set<int>> m, string friendly_name);

bool get_first_set(int key, int active_key, map<int, vector<vector<int>>> &prod, map<int, set<int>> &first);

void get_follow_set(map<int, vector<vector<int>>> &prod, map<int, set<int>> &follow,
                    map<int, set<int>> &first);

// Simple boolean to determine printing
const bool PRINT_DEBUG = false;
const bool PRINT_FiFoTable = true;

// Incase epsilon needs to be changed later
const int EPSILON = 0;

/**
 * Main method for our LL(1) parser.
 * @return 0
 */
int main() {

    cout << "================================================================================" << endl;
    cout << "                               Project 2: Part 1" << endl;
    cout << "                         Kevin Bohinski & Evan Melquist" << endl;
    cout << "                         CSC 435: Programming Languages" << endl;
    cout << "================================================================================" << endl;
    cout << "Note: It is recommended to use file redirection when running this." << endl;
    cout << "Ex: g++ -std=c++11 main.cpp -o a.exe && cat input.txt | .\\a.exe" << endl;

    cout << endl << "Attempting to parse input from stdin..." << endl;

    // Map to store terminals
    map<string, int> terminals;

    /*
     * Example terminal:
     * terminals[tok_eof] = 1
     */

    // Variables for parsing input
    string line;
    string token_name;
    int token_id;
    int max_token_id = 0;

    while (true) {
        getline(cin, line);

        if (line == "") {
            break;
        }

        stringstream ss(line);

        ss >> token_name;
        ss >> token_id;

        terminals[token_name] = token_id;
        max_token_id = max(max_token_id, token_id);
    }

    // Map to store non terminals and their id's in
    map<string, vector<vector<string>>> non_terminals;
    map<string, int> non_terminal_id;

    /*
     * Example non terminal:
     * non_terminals[stmt] = {ident, becomes, expr, }{rw_read, ident, }{rw_write, expr, }
     * non_terminal_id[stmt] = 3
     */

    // Variables for parsing input
    string lhs;
    string tmp;
    vector<vector<string>> rhs;
    int n = 1;

    while (true) {
        getline(cin, line);
        stringstream ss(line);
        vector<string> v;

        if (line == "") {
            break;
        }

        ss >> lhs;
        ss >> tmp;

        // If this is the non terminal's first appearance, give it an id
        if (non_terminal_id.find(lhs) == non_terminal_id.end()) {
            non_terminal_id[lhs] = n++;
        }

        if (tmp == "->") {
            while (ss >> tmp) {
                v.push_back(tmp);
            }
            rhs.push_back(v);
            non_terminals[lhs].push_back(v);
        }
    }

    // Map to store the productions in
    map<int, vector<vector<int>>> productions;

    /*
     * Example production:
     * productions[3] = {-2, -21, 4, }{-13, -2, }{-18, 4, }
     */

    for (auto k : non_terminal_id) {
        vector<vector<int>> value;
        for (auto v : non_terminals[k.first]) {
            vector<int> tmp_vec;
            for (auto vv : v) {
                if (non_terminal_id.find(vv) != non_terminal_id.end()) {
                    tmp_vec.push_back(non_terminal_id[vv]);
                } else {
                    // If it is a terminal, make it negative to avoid collision and to simplify further processing.
                    tmp_vec.push_back(-terminals[vv]);
                }
            }
            value.push_back(tmp_vec);
        }
        productions[k.second] = value;
    }

    // Map to store first and follow sets in
    map<int, set<int>> first_sets;
    map<int, set<int>> follow_sets;

    // Per follow set generation algorithm, set the follow set for the program start to epsilon
    follow_sets[1].insert(EPSILON);

    /*
     * Example first and follow set:
     * first_sets[1] = 2, 13, 18, 1,
     * follow_sets[3] = 2, 13, 18, 1, 0,
     */

    // Generate first sets
    for (auto k : productions) {
        get_first_set(k.first, k.first, productions, first_sets);
    }
    for (auto k : first_sets) {
        k.second.insert(EPSILON);
    }

    // Generate follow sets
    get_follow_set(productions, follow_sets, first_sets);

    // 2D array to store the parse table in
    int x_size = non_terminals.size();
    int y_size = max_token_id;
    int parse_table[x_size][y_size];

    // Alg based on http://web.cs.wpi.edu/~kal/PLT/PLT4.3.html
    for (int x = 0; x < x_size; x++) {
        for (int y = 0; y < y_size; y++) {
            parse_table[x][y] = 0;
        }
    }

    int alpha_n = 1, temp;
    for (auto p : productions) {
        int A = p.first - 1;
        for (auto v : p.second) {
            bool one = true;
            if (v.empty()) {
                for (auto b : follow_sets[A]) {
                    if (b != 0) {
                        parse_table[A][b - 1] = alpha_n;
                    }
                }
            }
            for (auto alpha : v) {
                if (one) {
                    one = false;
                    if (alpha < 0) {
                        temp = -1 * alpha;
                        parse_table[A][temp - 1] = alpha_n;
                    }
                    if (first_sets.find(alpha) != first_sets.end()) {
                        for (auto b : first_sets[alpha]) {
                            if (b != 0)
                                parse_table[A][b - 1] = alpha_n;
                        }
                    }
                    if (first_sets[alpha].find(EPSILON) != first_sets[alpha].end()) {
                        for (auto b : follow_sets[A]) {
                            if (b != 0) {
                                parse_table[A][b - 1] = alpha_n;
                            }
                        }
                    }
                }
            }
            alpha_n++;
        }
    }

    // Remove any first_set or follow_set entries that are not within bounds
    // This is caused by the parse table algorithm searching for sets that may not exist
    for (auto p : first_sets) {
        if (p.first > productions.size() || p.first < 1) {
            first_sets.erase(p.first);
        }
    }
    for (auto p : follow_sets) {
        if (p.first > productions.size() || p.first < 1) {
            follow_sets.erase(p.first);
        }
    }

    cout << "Success!" << endl << endl;

    cout << "Attempting to print generated data from input..." << endl;

    if (PRINT_DEBUG) {
        print_map_string_int(terminals, "terminals");
        print_map_string_vv_string(non_terminals, "non_terminals");
        print_map_string_int(non_terminal_id, "non_terminal_id");
        print_map_int_vv_int(productions, "productions");
    }

    if (PRINT_FiFoTable) {
        print_map_int_s_int(first_sets, "first_sets");
        print_map_int_s_int(follow_sets, "follow_sets");
        cout << "parse_table: " << endl;
        for (int x = 0; x < x_size; x++) {
            for (int y = 0; y < y_size; y++) {
                cout << parse_table[x][y] << ",";
                if (parse_table[x][y] < 10)
                    cout << " ";
                cout << " ";
            }
            cout << endl;
        }
    }

    cout << "Success!" << endl;

    cout << endl << "Attempting to use the parse_table to drive via a parse stack..." << endl;

    // Using the algorithm from p 83 of book...
    stack<string> parse_stack;
    queue<scanner::token> parse_queue;
    set<string> possible_tokens;

    for (int i = 0; i < 40; i++) {
        possible_tokens.insert(string(scanner::token_names[i]));
    }

    parse_stack.push("program");
    cout << "initial stack contents" << endl;

    // Consume everything from stdin
    scanner::token t;
    do {
        t = scanner::scan();
        parse_queue.push(t);
    } while (t.num != scanner::tok_eof);

    bool goes_to_ep = false;

    while (!parse_stack.empty()) {
        string top = parse_stack.top();
        parse_stack.pop();

        if (parse_queue.empty()) {
            if (top == "tok_eof") {
                cout << "Success, exiting!" << endl;
                return 0;
            }
            goes_to_ep = false;
            for (auto p : productions[non_terminal_id[top]]) {
                if (p.empty()) {
                    goes_to_ep = true;
                }
            }
            if (goes_to_ep) {
                cout << "predict " << top << " -> ep" << endl;
            } else {
                cerr << "Err: Unexpected token... Attempting to continue... (1)" << endl;
            }
        } else if (terminals.count(top) > 0) {
            if (parse_queue.front().name == top) {
                cout << "match " << parse_queue.front().name << endl;
                parse_queue.pop();
            } else {
                cerr << "Err: Unexpected token... Attempting to continue... (2)" << endl;
            }
        } else {
            int prediction = parse_table[non_terminal_id[top] - 1][terminals[parse_queue.front().name] - 1];
            if (prediction == 0) {
                // Cases 1, 2, 3 from syntax error recovery on spec
                if (parse_queue.front().name == "tok_error" ||
                    possible_tokens.find(parse_queue.front().name) == possible_tokens.end()) {
                    cerr << "Err: Error Recovery Case 1" << endl;
                    parse_queue.pop();
                    continue;
                } else if (terminals.count(parse_stack.top()) > 0 && parse_queue.front().name != parse_stack.top()) {
                    cout << "Err: Error Recovery Case 2..." << endl;
                    parse_stack.pop();
                } else if (terminals.find(top) == terminals.end()) {
                    cerr << "Err: Error Recovery Case 3...";
                    while (true) {
                        int t = parse_queue.front().num;
                        parse_queue.pop();
                        if (follow_sets[non_terminal_id[top]].find(t) != follow_sets[non_terminal_id[top]].end()) {
                            parse_stack.pop();
                            break;
                        } else if (first_sets[non_terminal_id[top]].find(t) != first_sets[non_terminal_id[top]].end()) {
                            break;
                        }
                    }
                } else {
                    cout << "Err: Unexpected token... Attempting to continue... (4)" << endl;
                    cout << "PREDICTION 0: top: " << top << " x:" << non_terminal_id[top] - 1 << " y:"
                         << terminals[parse_queue.front().name] - 1 << endl;
                    continue;
                }
            } else {
                cout << "predict " << top << " -> ";
                vector<string> prod = rhs[prediction - 1];
                for (int i = prod.size() - 1; i >= 0; i--) {
                    parse_stack.push(prod[i]);
                    cout << prod[i] << " ";
                }
                cout << endl;
            }

        }
    }

    cout << "Done!" << endl;

    return 0;
}

/**
 * Helper function to print map<string, int>
 * @param m : Map to print
 * @param friendly_name : Name to use in printing
 */
void print_map_string_int(map<string, int> m, string friendly_name) {
    for (auto p : m) {
        cout << friendly_name << "[" << p.first << "] = " << p.second << endl;
    }
    cout << endl;
}

/**
 * Helper function to print map<string, vector<vector<string>>>
 * @param m : Map to print
 * @param friendly_name : Name to use in printing
 */
void print_map_string_vv_string(map<string, vector<vector<string>>> m, string friendly_name) {
    for (auto p : m) {
        cout << friendly_name << "[" << p.first << "] = ";
        for (auto v : p.second) {
            cout << "{";
            for (auto vv : v) {
                cout << vv << ", ";
            }
            cout << "}";
        }
        cout << endl;
    }
    cout << endl;
}

/**
 * Helper function to print map<int, vector<vector<int>>>
 * @param m : Map to print
 * @param friendly_name : Name to use in printing
 */
void print_map_int_vv_int(map<int, vector<vector<int>>> m, string friendly_name) {
    for (auto p : m) {
        cout << friendly_name << "[" << p.first << "] = ";
        for (auto v : p.second) {
            cout << "{";
            for (auto vv : v) {
                cout << vv << ", ";
            }
            cout << "}";
        }
        cout << endl;
    }
    cout << endl;
}

/**
 * Helper function to print map<int, set<int>>
 * @param m : Map to print
 * @param friendly_name : Name to use in printing
 */
void print_map_int_s_int(map<int, set<int>> m, string friendly_name) {
    for (auto p : m) {
        cout << friendly_name << "[" << p.first << "] = ";
        for (auto v : p.second) {
            cout << v << ", ";
        }
        cout << endl;
    }
    cout << endl;
}

/**
 * Recursive function to generate first sets from productions
 * @param curr_key    : The key of what set is being generated
 * @param active_key  : The key of the recursive set
 * @param prod        : The productions map
 * @param first       : The first sets map
 * @return            : Boolean if epsilon exists in all productions.
 */
bool get_first_set(int curr_key, int active_key, map<int, vector<vector<int>>> &prod, map<int, set<int>> &first) {
    bool epsilon_in_all_prods = true;
    for (auto k : prod[curr_key]) {
        bool all_epsilon = true;
        for (auto v : k) {
            if (v == EPSILON) {
                first[active_key].insert(EPSILON);
            } else if (v < EPSILON) {
                first[active_key].insert(-v);
                all_epsilon = false;
                break;
            } else if (get_first_set(v, active_key, prod, first)) {
                all_epsilon = false;
                break;
            }
        }
        if (all_epsilon) {
            first[active_key].insert(EPSILON);
            epsilon_in_all_prods = false;
        }
    }
    return epsilon_in_all_prods;
}

/**
 * Function to generate follow sets from productions and first sets.
 * @param prod    : The productions map
 * @param follow  : The follow sets map
 * @param first   : The completed first sets map
 */
void get_follow_set(map<int, vector<vector<int>>> &prod, map<int, set<int>> &follow,
                    map<int, set<int>> &first) {
    // Add first sets to follow sets
    follow[0].insert(1);
    follow[1].insert(1);

    for (auto p : prod) {
        for (auto vv : p.second) {
            bool one = true;
            int last = 0;
            for (auto i : vv) {
                if (one) {
                    one = false;
                } else if (0 <= last) {
                    if (i > 0) {
                        for (auto x : first[i])
                            follow[last].insert(x);
                    } else {
                        follow[last].insert(-i);
                    }
                }
                last = i;
            }
        }
    }

    // Add follow sets to other follow sets
    for (auto p : prod) {
        for (auto vv : p.second) {
            bool one = true;
            int last;
            for (auto i : vv) {
                if (one) {
                    one = false;
                } else if (i > 0 && last > 0) {
                    if (first[i].find(EPSILON) != first[i].end()) {
                        for (auto follow_i : follow[p.first])
                            follow[last].insert(follow_i);
                    }
                }
                last = i;
            }
            if (last > 0) {
                for (auto follow_i : follow[p.first])
                    follow[last].insert(follow_i);
            }
        }
    }
}