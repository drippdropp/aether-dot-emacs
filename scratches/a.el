(s-match-strings-all
 (rx (or "Mary" "Jane" "Sue"))
 "Mary, Jane, and Sue went to Mary's house")

'(("Mary") ("Jane") ("Sue") ("Mary"))

"\\(?:Jane\\|Mary\\|Sue\\)"

