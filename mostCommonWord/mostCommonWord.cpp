string mostCommonWord(unordered_set<string> bannedWords, const string& str)
{
    int n = str.size();
    vector<char> ignored = {',', ' ', ';', '.'}; // add more here
    unordered_map<string, int> wordCount;
    for (int i = 0; i != -1;) {
        // find the next punctuation mark
        auto it = std::find_first_of(str.begin() + i, str.end(), ignored.begin(), ignored.end());
        int nextEndpoint = (it == str.end()) ? n : it - str.begin();
        // substring
        string text = str.substr(i, nextEndpoint - i);
        // wrote this offhand, probably got some mistakes. Code to transform to lowercase
        std::transform(text.begin(), text.end(), text.begin(), [](unsigned char c){ std::to_lower(c); });
        // update dictionary
        if (!bannedWords.contains(s)) wordCount[s]++;
        // find the next alphabet
        it = std::find_if(str.begin() + nextEndpoint, str.end(), [](char c){ std::isalnum(c); });
        i = (it == str.end()) ? -1 : it - str.begin();
    }
    return std::max_element(wordCount.begin(), wordCount.end(); 
        [](auto& lhs, auto& rhs){return lhs.second < rhs.second;}
        )->first;
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