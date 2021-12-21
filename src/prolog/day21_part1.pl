dice_roll(Step, Dice1, Dice2, Dice3, Sum) :-
  Dice3 is 3 * Step,
  Dice2 is Dice3 - 1,
  Dice1 is Dice2 - 1,
  Sum is Dice1 + Dice2 + Dice3.

odd(X) :- X mod 2 =:= 1.

calc_score(Roll, Score, Pos, New_Score, New_Pos) :-
  X is Pos + Roll,
  Y is X mod 10,
  (  Y =:= 0
  -> New_Pos is 10
  ;  New_Pos is Y
  ),
  New_Score is Score + New_Pos.
  

game_round(Round, Score, Pos, New_score, New_pos) :-
  dice_roll(Round, _, _, _, Sum),
  calc_score(Sum, Score, Pos, New_score, New_pos).

rolls(X, Y) :- Y is (X - 1) * 3.
result(X, Y, Z) :- Z is X * Y.

game(Start, Rolls, P1_score, P1_pos, P2_score, P2_pos, New_p1_score, New_p1_pos, New_p2_score, New_p2_pos, Winner, Result) :-
  (  P1_score >= 1000
  -> Winner is 1, rolls(Start, Rolls), result(P2_score, Rolls, Result),
     New_p1_pos is P1_pos, New_p1_score is P1_score,
     New_p2_pos is P2_pos, New_p2_score is P2_score
  ; (  P2_score >= 1000
    -> Winner is 2, rolls(Start, Rolls), result(P1_score, Rolls, Result),
       New_p1_pos is P1_pos, New_p1_score is P1_score,
       New_p2_pos is P2_pos, New_p2_score is P2_score
    ;
      (  odd(Start)
      -> game_round(Start, P1_score, P1_pos, Next_p1_score, Next_p1_pos), Next_p2_score is P2_score, Next_p2_pos is P2_pos
      ;  game_round(Start, P2_score, P2_pos, Next_p2_score, Next_p2_pos), Next_p1_score is P1_score, Next_p1_pos is P1_pos
      ),
      game(Start + 1, Rolls,
        Next_p1_score, Next_p1_pos, Next_p2_score, Next_p2_pos,
        New_p1_score, New_p1_pos, New_p2_score, New_p2_pos,
        Winner, Result)
    )
  ).
