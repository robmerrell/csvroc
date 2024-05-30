module [
    parse,
]

delimiter = ","
quote = "\""
endOfLine = "\n"

# parse : Str -> List Str
parse = \doc ->
    parsed = walkDoc doc
    parsed.rows

walkDoc = \doc ->
    docLength = Str.countUtf8Bytes doc
    Str.walkUtf8WithIndex doc { currentField: "", currentRow: [], rows: [], quoteCount: 0 } \state, byte, index ->
        str = Result.withDefault (Str.fromUtf8 [byte]) ""

        # add the character to the current field accumulator if the next
        updatedState =
            if str == delimiter && state.quoteCount == 0 then
                # a delimiter has been hit
                { state & currentField: "", currentRow: List.append state.currentRow state.currentField }
            else if str == endOfLine && state.quoteCount == 0 then
                # end of the line
                currentRow = List.append state.currentRow state.currentField
                { state & currentField: "", currentRow: [], rows: List.append state.rows currentRow }
            else if (index + 1 == docLength) then
                # end of the document
                currentField = Str.concat state.currentField str
                currentRow = List.append state.currentRow currentField
                { state & currentField: "", currentRow: [], rows: List.append state.rows currentRow }
            else
                # accumulate characters
                currentField = Str.concat state.currentField str
                { state & currentField: currentField }

        dbg updatedState

        updatedState

# Simple comma separated fields
expect parse "one,two" == [["one", "two"]]

# New line at the end
expect parse "one,two\n" == [["one", "two"]]

# Multiple lines
expect parse "one,two\nthree,four" == [["one", "two"], ["three", "four"]]

# Quoted delimiter and quotes. Quotes are required because a delimiter and quotes are in the field value.
# It's a little easier to read without the escapes: """one"",",two
expect parse "\"\"\"one\"\",\",two" == [["\"one\",", "two"]]

# Quoted end of line

# Trailing comma

# Too many columns

# Too few columns

# Header line
