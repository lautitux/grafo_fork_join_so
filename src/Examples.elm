module Examples exposing (examples, examplesDict)

import Dict exposing (Dict)


examples : List ( String, String )
examples =
    [ ( "Basic", """L1: T1
FORK L3
L2: T2
QUIT
L3: T3
T4
""" )
    , ( "Medium", """Count=2
FORK LD
A
FORK JC
B
QUIT
JC: JOIN Count, LC
QUIT
LC: C
QUIT
LD: D
GOTO JC""" )
    , ( "Advanced", """ContF=2
ContG=3
FORK LA
FORK LD
H
FORK LJF
GOTO LJG
LD: D
FORK LE
GOTO LJF
QUIT
LE: E
GOTO LJG
LA: A
FORK LC
B
QUIT
LC: C
LJG: JOIN ContG, LG
QUIT
LG: G
QUIT
LJF: JOIN ContF, LF
QUIT
LF: F""" )
    , ( "Tutorial"
      , """; =========================================================
;                  WELCOME TO THE TUTORIAL
; =========================================================
; In this language, every line is either a counter,
; a process (node), a flow control command, or a label.

; 1. DEFINING COUNTERS
; Counters act as "checks" for joining parallel paths.
; Here we define a counter that requires 2 paths to complete.
Counter = 2

; 2. STARTING THE FLOW
; A word on its own is a node in the graph. It represents a process.
Start_Tutorial

; 3. CREATING PARALLEL PATHS (FORK)
; FORK creates a new thread that jumps to a label.
; The current thread simply moves to the next line.
FORK Path_B_Label

; 4. THE MAIN PATH (Path A)
; This is what the first thread does:
Process_A
GOTO Sync_Point ; Now we go wait for the other path.

; 5. THE SECOND PATH (Path B)
; We define a label followed by the process.
Path_B_Label:
Process_B
; After Process_B is done, it also heads to the Sync_Point.
GOTO Sync_Point

; 6. SYNCHRONIZING (JOIN)
; This is the most important part of graph logic.
; JOIN checks 'Counter'.
; - If it's 2: it becomes 1 and this thread stops (QUIT).
; - If it's 1: it jumps to 'Final_Step'.
Sync_Point:
    JOIN Counter, Final_Step
    QUIT ; This ensures the first thread to arrive stops here.

; 7. THE FINAL RESULT
; Only the last thread to hit the JOIN will reach this label.
Final_Step:
    Tutorial_Complete

; 8. ENDING EXECUTION
; QUIT stops the current flow.
QUIT

; 9. Press the [RUN] button to view the rendered graph
"""
      )
    ]


examplesDict : Dict String String
examplesDict =
    Dict.fromList examples
