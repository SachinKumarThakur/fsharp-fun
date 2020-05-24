string mostCommonWord(const string& str, vector<string> bannedWords)
{
    // init hash set from vector
    unordered_set<string> banned(bannedWords.begin(), bannedWords.end());
    int len = int(str.size());
    vector<char> ignored = {',', ' ', ';', '.', '\'', '?', '!'}; // add more here
    unordered_map<string, int> wordCount;
    for (int idx = 0; idx != -1;) {
        // find the next punctuation mark
        auto it = std::find_first_of(str.begin() + idx, str.end(), ignored.begin(), ignored.end());
        int nextEndpoint = (it == str.end()) ? len : int(it - str.begin());
        // substring
        string text = str.substr(idx, nextEndpoint - idx);
        // wrote this offhand, probably got some mistakes. Code to transform to lowercase
        std::transform(text.begin(), text.end(), text.begin(), ::tolower);
        // update dictionary
        if (banned.find(text) == banned.end()) {
            ++wordCount[text];
        }
        // find the next alphabetical char
        it = std::find_if(str.begin() + nextEndpoint, str.end(), ::isalpha);
        // advance i to the position of the next alphabetical char
        idx = (it == str.end()) ? -1 : int(it - str.begin());
    }
    // return an iterator pointing to the word with highest count
    auto it = std::max_element(wordCount.begin(), wordCount.end(),
              [] (auto& lhs, auto& rhs) {
                return lhs.second < rhs.second;
              }
             );
    return it->first;
}

/* F# code, for reference
let mostCommonWordOnePass (bannedWords: Set<string>) (str: string) =
    // create a one-pass lexer to identify words
    // Lex grammar:
    //   [a-zA-Z]*    return Ident;
    //   [ ,]*        ;
    let n = String.length str
    let rec splitter i lst =
        if i <> -1 then
            let nextIgnored = str.IndexOfAny(ignored, i)
            let nextEndpt =
                if nextIgnored = -1 then n else nextIgnored
            let s = str.Substring(i, nextEndpt - i)
            let nextAlphabet = str.IndexOfAny(alphabet, nextEndpt)
            if Set.contains s bannedWords then
                splitter nextAlphabet lst  // don't include s, it's banned
            else
                splitter nextAlphabet (s :: lst)
        else lst
    splitter 0 []
    |> List.countBy id
    |> List.maxBy snd
    |> fst
*/
