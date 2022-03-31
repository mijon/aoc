
input ← ⊃⎕NGET '../inputs/1_input.txt' 0
input ←input[⍳7000] ⍝ Needed to remove new line


part1 ← +/¯1*')'=⊢
part2 ← ¯1⍳⍨(+\¯1*')'=⊢)
