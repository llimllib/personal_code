       IDENTIFICATION DIVISION.
       PROGRAM-ID. ADVENT-2023-12.

       ENVIRONMENT DIVISION.
       INPUT-OUTPUT SECTION.
       FILE-CONTROL.
           SELECT INPUT-FILE ASSIGN TO "/dev/stdin"
               ORGANIZATION IS LINE SEQUENTIAL
               FILE STATUS IS WS-FILE-STATUS.

       DATA DIVISION.
       FILE SECTION.
       FD INPUT-FILE.
       01 INPUT-RECORD              PIC X(1024).

       WORKING-STORAGE SECTION.
       01 WS-FILE-STATUS            PIC XX.
       01 WS-LINE                   PIC X(1024).
       01 WS-LINE-LEN               PIC 9(4).
       01 WS-SPACE-POS              PIC 9(4).
       01 WS-LENS-PART              PIC X(512).
       01 WS-LENS-LEN               PIC 9(4).
       01 WS-PARSE-POS              PIC 9(4).
       01 WS-NUM-START              PIC 9(4).
       01 WS-NUM-STR                PIC X(10).
       01 WS-NUM-VAL                PIC 9(4).
       01 WS-PUZZLE-COUNT           PIC 9(4) VALUE 0.
       01 WS-PUZZLES.
           05 WS-PUZZLE OCCURS 1100 TIMES.
               10 WS-PZ-COND        PIC X(200).
               10 WS-PZ-COND-LEN    PIC 9(4).
               10 WS-PZ-MACH-CNT    PIC 9(4).
               10 WS-PZ-MACH        PIC 9(4) OCCURS 50 TIMES.

      *> Current count call parameters
       01 WS-CUR-S                  PIC X(1024).
       01 WS-CUR-S-LEN             PIC 9(4).
       01 WS-CUR-MC                PIC 9(4).
       01 WS-CUR-MV                PIC 9(4) OCCURS 100 TIMES.
       01 WS-CUR-RESULT            PIC 9(18).

      *> Recursion stack
       01 WS-SP                    PIC 9(4) VALUE 0.
       01 WS-MAX-STACK             PIC 9(4) VALUE 500.
       01 WS-STACK.
           05 WS-STK OCCURS 500 TIMES.
               10 WS-STK-S         PIC X(200).
               10 WS-STK-SL        PIC 9(4).
               10 WS-STK-MC        PIC 9(4).
               10 WS-STK-MV        PIC 9(4) OCCURS 100 TIMES.
               10 WS-STK-CI        PIC 9(4).
               10 WS-STK-CM        PIC 9(4).
               10 WS-STK-CN        PIC 9(18).
               10 WS-STK-CL        PIC 9(4).
               10 WS-STK-CMCH      PIC 9(4).

      *> State machine control
       01 WS-STATE                 PIC 9 VALUE 0.
      *>   0 = entry (check cache, base case, init loop)
      *>   1 = outer loop iteration
      *>   2 = return from recursive call
      *>   9 = done

      *> Loop locals
       01 WS-CI                    PIC 9(4).
       01 WS-CL                    PIC 9(4).
       01 WS-CM                    PIC 9(4).
       01 WS-CN                    PIC 9(18).
       01 WS-CMATCH                PIC 9(4).
       01 WS-HAS-HASH              PIC 9.
       01 WS-CHK-POS               PIC 9(4).
       01 WS-CDIFF                 PIC 9(4).
       01 WS-CCHAR                 PIC X.
       01 WS-INNER-DIFF            PIC 9(4).
       01 WS-INNER-DONE            PIC 9.
       01 WS-IP1                   PIC 9(4).
       01 WS-IP2                   PIC 9(4).
       01 WS-TEMP                  PIC 9(4).
       01 WS-LOOP-LIMIT            PIC 9(4).

      *> Substring temp
       01 WS-SUB-S                 PIC X(1024).
       01 WS-SUB-LEN               PIC 9(4).

      *> Cache (open addressing hash table)
       01 WS-CACHE-CAP             PIC 9(8) VALUE 262144.
       01 WS-CACHE-CNT             PIC 9(8) VALUE 0.
       01 WS-CACHE.
           05 WS-CS OCCURS 262144 TIMES.
               10 WS-CS-USED       PIC 9 VALUE 0.
               10 WS-CS-HASH       PIC 9(18).
               10 WS-CS-S          PIC X(200).
               10 WS-CS-SL         PIC 9(4).
               10 WS-CS-MC         PIC 9(4).
               10 WS-CS-MV         PIC 9(4) OCCURS 100 TIMES.
               10 WS-CS-RES        PIC 9(18).
       01 WS-HV                    PIC 9(18).
       01 WS-HI                    PIC 9(4).
       01 WS-HS                    PIC 9(8).
       01 WS-HF                    PIC 9.
       01 WS-HP                    PIC 9(8).
       01 WS-HM                    PIC 9.

      *> Unfold
       01 WS-UF-S                  PIC X(1024).
       01 WS-UF-SL                 PIC 9(4).
       01 WS-UF-MC                 PIC 9(4).
       01 WS-UF-MV                 PIC 9(4) OCCURS 100 TIMES.
       01 WS-UF-POS                PIC 9(4).
       01 WS-UF-J                  PIC 9(4).
       01 WS-UF-K                  PIC 9(4).
       01 WS-UF-COPY               PIC 9(4).
       01 WS-UF-TPOS               PIC 9(4).

      *> Totals
       01 WS-SUM1                  PIC 9(18) VALUE 0.
       01 WS-SUM2                  PIC 9(18) VALUE 0.
       01 WS-PI                    PIC 9(4).
       01 WS-J                     PIC 9(4).
       01 WS-DISPLAY-NUM           PIC Z(17)9.
       01 WS-TRIMMED               PIC X(1024).
       01 WS-CLR-IDX               PIC 9(8).

       PROCEDURE DIVISION.
       MAIN-PROGRAM.
           PERFORM READ-INPUT
           PERFORM SOLVE-PART1
           MOVE WS-SUM1 TO WS-DISPLAY-NUM
           DISPLAY FUNCTION TRIM(WS-DISPLAY-NUM)
           PERFORM SOLVE-PART2
           MOVE WS-SUM2 TO WS-DISPLAY-NUM
           DISPLAY FUNCTION TRIM(WS-DISPLAY-NUM)
           STOP RUN.

       READ-INPUT.
           OPEN INPUT INPUT-FILE
           READ INPUT-FILE INTO WS-LINE
           PERFORM UNTIL WS-FILE-STATUS NOT = "00"
               ADD 1 TO WS-PUZZLE-COUNT
               PERFORM PARSE-LINE
               READ INPUT-FILE INTO WS-LINE
           END-PERFORM
           CLOSE INPUT-FILE
           .

       PARSE-LINE.
           MOVE FUNCTION TRIM(WS-LINE) TO WS-TRIMMED
           MOVE FUNCTION LENGTH(FUNCTION TRIM(WS-LINE))
               TO WS-LINE-LEN
           MOVE WS-TRIMMED TO WS-LINE
           MOVE 0 TO WS-SPACE-POS
           PERFORM VARYING WS-PARSE-POS FROM 1 BY 1
               UNTIL WS-PARSE-POS > WS-LINE-LEN
               IF WS-LINE(WS-PARSE-POS:1) = " "
                   MOVE WS-SPACE-POS TO WS-SPACE-POS
                   MOVE WS-PARSE-POS TO WS-SPACE-POS
                   EXIT PERFORM
               END-IF
           END-PERFORM
           SUBTRACT 1 FROM WS-SPACE-POS GIVING WS-NUM-VAL
           MOVE WS-LINE(1:WS-NUM-VAL)
               TO WS-PZ-COND(WS-PUZZLE-COUNT)
           MOVE WS-NUM-VAL
               TO WS-PZ-COND-LEN(WS-PUZZLE-COUNT)
           ADD 1 TO WS-SPACE-POS GIVING WS-PARSE-POS
           SUBTRACT WS-PARSE-POS FROM WS-LINE-LEN
               GIVING WS-LENS-LEN
           ADD 1 TO WS-LENS-LEN
           MOVE WS-LINE(WS-PARSE-POS:WS-LENS-LEN)
               TO WS-LENS-PART
           MOVE WS-LENS-LEN TO WS-LINE-LEN
           MOVE 0 TO WS-PZ-MACH-CNT(WS-PUZZLE-COUNT)
           MOVE 1 TO WS-NUM-START
           PERFORM VARYING WS-PARSE-POS FROM 1 BY 1
               UNTIL WS-PARSE-POS > WS-LINE-LEN
               IF WS-LENS-PART(WS-PARSE-POS:1) = ","
                   OR WS-PARSE-POS = WS-LINE-LEN
                   IF WS-LENS-PART(WS-PARSE-POS:1) = ","
                       SUBTRACT WS-NUM-START FROM WS-PARSE-POS
                           GIVING WS-NUM-VAL
                       MOVE WS-LENS-PART(
                           WS-NUM-START:WS-NUM-VAL)
                           TO WS-NUM-STR
                   ELSE
                       SUBTRACT WS-NUM-START FROM WS-PARSE-POS
                           GIVING WS-NUM-VAL
                       ADD 1 TO WS-NUM-VAL
                       MOVE WS-LENS-PART(
                           WS-NUM-START:WS-NUM-VAL)
                           TO WS-NUM-STR
                   END-IF
                   ADD 1 TO WS-PZ-MACH-CNT(WS-PUZZLE-COUNT)
                   MOVE FUNCTION NUMVAL(
                       FUNCTION TRIM(WS-NUM-STR))
                       TO WS-PZ-MACH(WS-PUZZLE-COUNT,
                       WS-PZ-MACH-CNT(WS-PUZZLE-COUNT))
                   ADD 1 TO WS-PARSE-POS GIVING WS-NUM-START
               END-IF
           END-PERFORM
           .

       SOLVE-PART1.
           MOVE 0 TO WS-SUM1
           PERFORM CLEAR-CACHE
           PERFORM VARYING WS-PI FROM 1 BY 1
               UNTIL WS-PI > WS-PUZZLE-COUNT
               MOVE WS-PZ-COND-LEN(WS-PI) TO WS-CUR-S-LEN
               MOVE SPACES TO WS-CUR-S
               IF WS-CUR-S-LEN > 0
                   MOVE WS-PZ-COND(WS-PI)(1:WS-CUR-S-LEN)
                       TO WS-CUR-S(1:WS-CUR-S-LEN)
               END-IF
               MOVE WS-PZ-MACH-CNT(WS-PI) TO WS-CUR-MC
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-CUR-MC
                   MOVE WS-PZ-MACH(WS-PI, WS-J)
                       TO WS-CUR-MV(WS-J)
               END-PERFORM
               PERFORM DO-COUNT
               ADD WS-CUR-RESULT TO WS-SUM1
           END-PERFORM
           .

       SOLVE-PART2.
           MOVE 0 TO WS-SUM2
           PERFORM CLEAR-CACHE
           PERFORM VARYING WS-PI FROM 1 BY 1
               UNTIL WS-PI > WS-PUZZLE-COUNT
               PERFORM UNFOLD-PUZZLE
               MOVE WS-UF-SL TO WS-CUR-S-LEN
               MOVE SPACES TO WS-CUR-S
               IF WS-CUR-S-LEN > 0
                   MOVE WS-UF-S(1:WS-CUR-S-LEN)
                       TO WS-CUR-S(1:WS-CUR-S-LEN)
               END-IF
               MOVE WS-UF-MC TO WS-CUR-MC
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-CUR-MC
                   MOVE WS-UF-MV(WS-J) TO WS-CUR-MV(WS-J)
               END-PERFORM
               PERFORM DO-COUNT
               ADD WS-CUR-RESULT TO WS-SUM2
           END-PERFORM
           .

       UNFOLD-PUZZLE.
           MOVE SPACES TO WS-UF-S
           MOVE WS-PZ-COND-LEN(WS-PI) TO WS-UF-POS
           IF WS-UF-POS > 0
               MOVE WS-PZ-COND(WS-PI)(1:WS-UF-POS)
                   TO WS-UF-S(1:WS-UF-POS)
           END-IF
           PERFORM 4 TIMES
               ADD 1 TO WS-UF-POS
               MOVE "?" TO WS-UF-S(WS-UF-POS:1)
               MOVE WS-PZ-COND-LEN(WS-PI) TO WS-UF-J
               ADD 1 TO WS-UF-POS GIVING WS-UF-TPOS
               IF WS-UF-J > 0
                   MOVE WS-PZ-COND(WS-PI)(1:WS-UF-J)
                       TO WS-UF-S(WS-UF-TPOS:WS-UF-J)
               END-IF
               ADD WS-UF-J TO WS-UF-POS
           END-PERFORM
           MOVE WS-UF-POS TO WS-UF-SL
           MULTIPLY WS-PZ-MACH-CNT(WS-PI) BY 5
               GIVING WS-UF-MC
           MOVE 0 TO WS-UF-K
           PERFORM 5 TIMES
               PERFORM VARYING WS-UF-COPY FROM 1 BY 1
                   UNTIL WS-UF-COPY >
                       WS-PZ-MACH-CNT(WS-PI)
                   ADD 1 TO WS-UF-K
                   MOVE WS-PZ-MACH(WS-PI, WS-UF-COPY)
                       TO WS-UF-MV(WS-UF-K)
               END-PERFORM
           END-PERFORM
           .

       CLEAR-CACHE.
           PERFORM VARYING WS-CLR-IDX FROM 1 BY 1
               UNTIL WS-CLR-IDX > WS-CACHE-CAP
               MOVE 0 TO WS-CS-USED(WS-CLR-IDX)
           END-PERFORM
           MOVE 0 TO WS-CACHE-CNT
           .

       COMPUTE-HASH.
           MOVE 5381 TO WS-HV
           IF WS-CUR-S-LEN > 0
               PERFORM VARYING WS-HI FROM 1 BY 1
                   UNTIL WS-HI > WS-CUR-S-LEN
                   COMPUTE WS-HV = FUNCTION MOD(
                       WS-HV * 33 +
                       FUNCTION ORD(WS-CUR-S(WS-HI:1)),
                       999999999999999937)
               END-PERFORM
           END-IF
           COMPUTE WS-HV = FUNCTION MOD(
               WS-HV * 33 + WS-CUR-MC,
               999999999999999937)
           IF WS-CUR-MC > 0
               PERFORM VARYING WS-HI FROM 1 BY 1
                   UNTIL WS-HI > WS-CUR-MC
                   COMPUTE WS-HV = FUNCTION MOD(
                       WS-HV * 33 + WS-CUR-MV(WS-HI),
                       999999999999999937)
               END-PERFORM
           END-IF
           .

       CACHE-LOOKUP.
           PERFORM COMPUTE-HASH
           COMPUTE WS-HS = FUNCTION MOD(
               WS-HV, WS-CACHE-CAP) + 1
           MOVE 0 TO WS-HF
           MOVE 0 TO WS-HP
           PERFORM UNTIL WS-HP >= WS-CACHE-CAP
               IF WS-CS-USED(WS-HS) = 0
                   EXIT PERFORM
               END-IF
               IF WS-CS-HASH(WS-HS) = WS-HV
                   AND WS-CS-SL(WS-HS) = WS-CUR-S-LEN
                   AND WS-CS-MC(WS-HS) = WS-CUR-MC
                   MOVE 1 TO WS-HM
                   IF WS-CUR-S-LEN > 0
                       IF WS-CS-S(WS-HS)(1:WS-CUR-S-LEN)
                           NOT = WS-CUR-S(1:WS-CUR-S-LEN)
                           MOVE 0 TO WS-HM
                       END-IF
                   END-IF
                   IF WS-HM = 1
                       MOVE 1 TO WS-HF
                       MOVE WS-CS-RES(WS-HS) TO WS-CUR-RESULT
                       EXIT PERFORM
                   END-IF
               END-IF
               ADD 1 TO WS-HS
               IF WS-HS > WS-CACHE-CAP
                   MOVE 1 TO WS-HS
               END-IF
               ADD 1 TO WS-HP
           END-PERFORM
           .

       CACHE-STORE.
           IF WS-CACHE-CNT < WS-CACHE-CAP
               MOVE 1 TO WS-CS-USED(WS-HS)
               MOVE WS-HV TO WS-CS-HASH(WS-HS)
               IF WS-CUR-S-LEN > 0
                   MOVE WS-CUR-S(1:WS-CUR-S-LEN)
                       TO WS-CS-S(WS-HS)(1:WS-CUR-S-LEN)
               END-IF
               MOVE WS-CUR-S-LEN TO WS-CS-SL(WS-HS)
               MOVE WS-CUR-MC TO WS-CS-MC(WS-HS)
               IF WS-CUR-MC > 0
                   PERFORM VARYING WS-HI FROM 1 BY 1
                       UNTIL WS-HI > WS-CUR-MC
                       MOVE WS-CUR-MV(WS-HI)
                           TO WS-CS-MV(WS-HS, WS-HI)
                   END-PERFORM
               END-IF
               MOVE WS-CUR-RESULT TO WS-CS-RES(WS-HS)
               ADD 1 TO WS-CACHE-CNT
           END-IF
           .

      *> ============================================================
      *> Iterative count using explicit state machine and stack.
      *> This avoids nested PERFORMs which cause stack overflow.
      *> ============================================================
       DO-COUNT.
           MOVE 0 TO WS-SP
           MOVE 0 TO WS-STATE
           PERFORM UNTIL WS-STATE = 9
               EVALUATE WS-STATE
                   WHEN 0 PERFORM STATE-ENTRY
                   WHEN 1 PERFORM STATE-OUTER-LOOP
                   WHEN 2 PERFORM STATE-RESUME
               END-EVALUATE
           END-PERFORM
           .

      *> State 0: Entry - check cache, handle base case, init loop
       STATE-ENTRY.
           PERFORM CACHE-LOOKUP
           IF WS-HF = 1
               PERFORM POP-OR-DONE
           ELSE
               IF WS-CUR-MC = 0
                   MOVE 0 TO WS-HAS-HASH
                   IF WS-CUR-S-LEN > 0
                       PERFORM VARYING WS-CHK-POS FROM 1 BY 1
                           UNTIL WS-CHK-POS > WS-CUR-S-LEN
                           IF WS-CUR-S(WS-CHK-POS:1) = "#"
                               MOVE 1 TO WS-HAS-HASH
                               EXIT PERFORM
                           END-IF
                       END-PERFORM
                   END-IF
                   IF WS-HAS-HASH = 0
                       MOVE 1 TO WS-CUR-RESULT
                   ELSE
                       MOVE 0 TO WS-CUR-RESULT
                   END-IF
                   PERFORM FIND-EMPTY-SLOT
                   PERFORM CACHE-STORE
                   PERFORM POP-OR-DONE
               ELSE
                   MOVE WS-CUR-S-LEN TO WS-CL
                   MOVE WS-CUR-MV(1) TO WS-CM
                   MOVE 0 TO WS-CN
                   MOVE 0 TO WS-CI
                   MOVE 1 TO WS-STATE
               END-IF
           END-IF
           .

      *> State 1: One iteration of the outer while loop
       STATE-OUTER-LOOP.
           COMPUTE WS-LOOP-LIMIT = WS-CL + WS-CM
           IF WS-CI >= WS-LOOP-LIMIT
               PERFORM FINISH-LOOP
           ELSE
               MOVE 0 TO WS-HAS-HASH
               IF WS-CI > 0
                   PERFORM VARYING WS-CHK-POS FROM 1 BY 1
                       UNTIL WS-CHK-POS > WS-CI
                       IF WS-CUR-S(WS-CHK-POS:1) = "#"
                           MOVE 1 TO WS-HAS-HASH
                           EXIT PERFORM
                       END-IF
                   END-PERFORM
               END-IF
               IF WS-HAS-HASH = 1
                   PERFORM FINISH-LOOP
               ELSE
                   MOVE WS-CI TO WS-CMATCH
                   MOVE 0 TO WS-INNER-DONE
                   PERFORM UNTIL WS-INNER-DONE = 1
                       IF WS-CI >= WS-CL
                           MOVE 1 TO WS-INNER-DONE
                       ELSE
                           ADD 1 TO WS-CI GIVING WS-IP1
                           MOVE WS-CUR-S(WS-IP1:1) TO WS-CCHAR
                           SUBTRACT WS-CMATCH FROM WS-CI
                               GIVING WS-INNER-DIFF
                           IF (WS-CCHAR = "?"
                               OR WS-CCHAR = "#")
                               AND WS-INNER-DIFF < WS-CM
                               ADD 1 TO WS-CI
                           ELSE
                               MOVE 1 TO WS-INNER-DONE
                           END-IF
                       END-IF
                   END-PERFORM
                   SUBTRACT WS-CMATCH FROM WS-CI
                       GIVING WS-CDIFF
                   IF WS-CDIFF = WS-CM
                       IF WS-CI = WS-CL
                           PERFORM PUSH-AND-RECURSE
                       ELSE
                           ADD 1 TO WS-CI GIVING WS-IP1
                           IF WS-CUR-S(WS-IP1:1) = "."
                               OR WS-CUR-S(WS-IP1:1) = "?"
                               PERFORM PUSH-AND-RECURSE
                           ELSE
                               ADD 1 TO WS-CMATCH GIVING WS-CI
                           END-IF
                       END-IF
                   ELSE
                       ADD 1 TO WS-CMATCH GIVING WS-CI
                   END-IF
               END-IF
           END-IF
           .

      *> State 2: Resume after recursive call returned
       STATE-RESUME.
           ADD WS-CUR-RESULT TO WS-CN
           ADD 1 TO WS-CMATCH GIVING WS-CI
           MOVE 1 TO WS-STATE
           .

      *> Finish the outer loop - store result and return
       FINISH-LOOP.
           MOVE WS-CN TO WS-CUR-RESULT
           PERFORM FIND-EMPTY-SLOT
           PERFORM CACHE-STORE
           PERFORM POP-OR-DONE
           .

      *> Find an empty cache slot for storing
       FIND-EMPTY-SLOT.
           PERFORM COMPUTE-HASH
           COMPUTE WS-HS = FUNCTION MOD(
               WS-HV, WS-CACHE-CAP) + 1
           PERFORM VARYING WS-HP FROM 0 BY 1
               UNTIL WS-HP >= WS-CACHE-CAP
               IF WS-CS-USED(WS-HS) = 0
                   EXIT PERFORM
               END-IF
               IF WS-CS-HASH(WS-HS) = WS-HV
                   AND WS-CS-SL(WS-HS) = WS-CUR-S-LEN
                   AND WS-CS-MC(WS-HS) = WS-CUR-MC
                   EXIT PERFORM
               END-IF
               ADD 1 TO WS-HS
               IF WS-HS > WS-CACHE-CAP
                   MOVE 1 TO WS-HS
               END-IF
           END-PERFORM
           .

      *> Save state and set up recursive call
       PUSH-AND-RECURSE.
           ADD 1 TO WS-SP
           IF WS-CUR-S-LEN > 0
               MOVE WS-CUR-S(1:WS-CUR-S-LEN)
                   TO WS-STK-S(WS-SP)(1:WS-CUR-S-LEN)
           END-IF
           MOVE WS-CUR-S-LEN TO WS-STK-SL(WS-SP)
           MOVE WS-CUR-MC TO WS-STK-MC(WS-SP)
           IF WS-CUR-MC > 0
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-CUR-MC
                   MOVE WS-CUR-MV(WS-J)
                       TO WS-STK-MV(WS-SP, WS-J)
               END-PERFORM
           END-IF
           MOVE WS-CI TO WS-STK-CI(WS-SP)
           MOVE WS-CMATCH TO WS-STK-CMCH(WS-SP)
           MOVE WS-CM TO WS-STK-CM(WS-SP)
           MOVE WS-CN TO WS-STK-CN(WS-SP)
           MOVE WS-CL TO WS-STK-CL(WS-SP)

      *>   Build new s = s[i+2:]
           IF WS-CI + 1 < WS-CL
               SUBTRACT WS-CI FROM WS-CL GIVING WS-SUB-LEN
               SUBTRACT 1 FROM WS-SUB-LEN
               IF WS-SUB-LEN > 0
                   ADD 2 TO WS-CI GIVING WS-IP2
                   MOVE SPACES TO WS-SUB-S
                   MOVE WS-CUR-S(WS-IP2:WS-SUB-LEN)
                       TO WS-SUB-S(1:WS-SUB-LEN)
                   MOVE SPACES TO WS-CUR-S
                   MOVE WS-SUB-S(1:WS-SUB-LEN)
                       TO WS-CUR-S(1:WS-SUB-LEN)
                   MOVE WS-SUB-LEN TO WS-CUR-S-LEN
               ELSE
                   MOVE 0 TO WS-CUR-S-LEN
                   MOVE SPACES TO WS-CUR-S
               END-IF
           ELSE
               MOVE 0 TO WS-CUR-S-LEN
               MOVE SPACES TO WS-CUR-S
           END-IF

      *>   Build new machines = machines[1:]
           SUBTRACT 1 FROM WS-CUR-MC
           IF WS-CUR-MC > 0
               PERFORM VARYING WS-J FROM 1 BY 1
                   UNTIL WS-J > WS-CUR-MC
                   ADD 1 TO WS-J GIVING WS-TEMP
                   MOVE WS-CUR-MV(WS-TEMP)
                       TO WS-CUR-MV(WS-J)
               END-PERFORM
           END-IF

           MOVE 0 TO WS-STATE
           .

      *> Pop from stack or mark done
       POP-OR-DONE.
           IF WS-SP = 0
               MOVE 9 TO WS-STATE
           ELSE
               MOVE WS-STK-SL(WS-SP) TO WS-CUR-S-LEN
               MOVE SPACES TO WS-CUR-S
               IF WS-CUR-S-LEN > 0
                   MOVE WS-STK-S(WS-SP)(1:WS-CUR-S-LEN)
                       TO WS-CUR-S(1:WS-CUR-S-LEN)
               END-IF
               MOVE WS-STK-MC(WS-SP) TO WS-CUR-MC
               IF WS-CUR-MC > 0
                   PERFORM VARYING WS-J FROM 1 BY 1
                       UNTIL WS-J > WS-CUR-MC
                       MOVE WS-STK-MV(WS-SP, WS-J)
                           TO WS-CUR-MV(WS-J)
                   END-PERFORM
               END-IF
               MOVE WS-STK-CI(WS-SP) TO WS-CI
               MOVE WS-STK-CMCH(WS-SP) TO WS-CMATCH
               MOVE WS-STK-CM(WS-SP) TO WS-CM
               MOVE WS-STK-CN(WS-SP) TO WS-CN
               MOVE WS-STK-CL(WS-SP) TO WS-CL
               SUBTRACT 1 FROM WS-SP
               MOVE 2 TO WS-STATE
           END-IF
           .

       END PROGRAM ADVENT-2023-12.
