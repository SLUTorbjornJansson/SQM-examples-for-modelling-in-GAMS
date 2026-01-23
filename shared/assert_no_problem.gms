$onText
    @purpose: Test utility. Assert that a set is empty, and if it is not,
              show it's contents, throw an exception and unload memory to a gdx file.
                
    @author: T. Jansson
    @usage: 
       $batinclude shared/assert_empty_set.gms init
       ...
       $batinclude shared/assert_empty_set.gms errorPar "errorMessage"
       
       where errorPar is the name of the parameter to be tested, and errorMessage is 
       a text string to be shown in case of an error.
    
$offText

$ifi not set scrdir $setLocal scrdir ..\output\temp

* --- Determine if the program should just declare symbols or actually process errors

$iftheni.mode "%1"=="init"

* --- Initialize error device

*   Give a file name for unloading data in case of errors
    $$setGlobal ERROR_FILE error.gdx

*   Declare symbols for reporting problems    
    scalar p_problemTol "Tolerance for problems" /1E-6/;

*   Declare generic parameters to hold set elements with problems
    parameter p_problem1D(*) "Data or set elements with a problem";
    parameter p_problem2D(*,*) "Data or set elements with a problem";
    parameter p_problem3D(*,*,*) "Data or set elements with a problem";
    parameter p_problem4D(*,*,*,*) "Data or set elements with a problem";
    parameter p_problem5D(*,*,*,*,*) "Data or set elements with a problem";

    option kill = p_problem1D;
    option kill = p_problem2D;
    option kill = p_problem3D;
    option kill = p_problem4D;
    option kill = p_problem5D;


$else.mode

* --- Check that a set is empty, otherwise unload data and abort with the error message provided by the caller

    $$setLocal errorSet %1
    $$setLocal errorMessage %2

    if(card(%errorSet%),
        execute_unload "%ERROR_FILE%";
        display "ERROR in file %system.IncParent%, line %system.IncParentL%. Data unloaded to %ERROR_FILE%.";
        display "%errorMessage%", %errorSet%;
        abort "Test failed: %errorMessage%. See listing for more details";
    );


$endif.mode
