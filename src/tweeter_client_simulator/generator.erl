-module(generator).
-define(MAX_SENTENCE_LENGTH, 10).
-define(TOTAL_WORDS, 100).
-define(WORDS_TABLE_NAME, words).

%% API
-export([
    generate_tweet/1,
    save_words/0
]).

save_words() ->
    Words = ["the","of","and","a","to","in","is","you","that","it","he","was","for","on","are","as","with","his","they","I","at","be","this","have","from","or","one","had","by","word","but","not","what","all","were","we","when","your","can","said","there","use","an","each","which","she","do","how","their","if","will","up","other","about","out","many","then","them","these","so","some","her","would","make","like","him","into","time","has","look","two","more","write","go","see","number","no","way","could","people","my","than","first","water","been","call","who","oil","its","now","find","long","down","day","did","get","come","made","may","part"],
    WordsZip = lists:zip(lists:seq(1, length(Words)), Words),
    TableOptions = [
        protected,
        named_table,
        {read_concurrency, true}
    ],
    ets:new(?WORDS_TABLE_NAME, TableOptions),
    ets:insert(?WORDS_TABLE_NAME, WordsZip).


generate_tweet(N) ->
    SentenceList = lists:map(fun (_) -> [{_, Word}] = ets:lookup(?WORDS_TABLE_NAME, rand:uniform(100)), Word  end, lists:seq(1, rand:uniform(?MAX_SENTENCE_LENGTH))),
    [{_, Hashtag}] = ets:lookup(?WORDS_TABLE_NAME, rand:uniform(100)),
    Mention = "@" ++ integer_to_list(rand:uniform(N)),
    lists:join(" ", SentenceList) ++  [ " #", Hashtag, " ", Mention].