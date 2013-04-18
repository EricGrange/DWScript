Procedure ExceptionHandler;
Begin

 // Outside re-raising
 Raise ExceptObject; // User defined exception: Floating point division by zero
                     // but actually it is re-raised runtime-initiated exception
End;

Try

 var x := 0;
 var y := 5 div x;

Except

 // Inside Re-rasing
 //Raise; // Floating point division by zero

 ExceptionHandler;

End;