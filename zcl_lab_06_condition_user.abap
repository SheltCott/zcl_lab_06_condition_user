CLASS zcl_lab_06_condition_user DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC .

  PUBLIC SECTION.
    INTERFACES if_oo_adt_classrun .
    DATA: mv_conditional TYPE i,
          mv_string      TYPE string,
          mv_counter     TYPE i,
          mv_string_2    TYPE string,
          mv_time        TYPE t,
          mv_counter_2   TYPE i,
          mt_employees   TYPE TABLE OF zemp_logali,
          ms_employee    TYPE zemp_logali,
          mv_exception   TYPE f VALUE 5.

    METHODS perform_if_endif IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_case_endcase IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_do_enddo IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_check IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_switch IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_cond IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_while_endwhile IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS perform_loop_endloop IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.
    METHODS get_case_result IMPORTING iv_string TYPE string RETURNING VALUE(rv_result) TYPE string.
    METHODS perform_try_endtry IMPORTING ir_out TYPE REF TO if_oo_adt_classrun_out.

  PROTECTED SECTION.
  PRIVATE SECTION.
ENDCLASS.

CLASS zcl_lab_06_condition_user IMPLEMENTATION.
  METHOD if_oo_adt_classrun~main.
*    IF / ENDIF
    me->perform_if_endif( out ).

*    CASE / ENDCASE
    me->perform_case_endcase( out ).

*    DO / ENDDO
    me->perform_do_enddo( out ).

*    CHECK
    me->perform_check( out ).

*    SWITCH
    me->perform_switch( out ).

*    COND
    me->perform_cond( out ).

*    WHILE / ENDWHILE
    me->perform_while_endwhile( out ).

*    LOOP / ENDLOOP
    me->perform_loop_endloop( out ).

*    TRY / ENDTRY
    me->perform_try_endtry( out ).
  ENDMETHOD.

  METHOD perform_if_endif.
*     Declare and assign the variable
    me->mv_conditional = 7.
*     Check if the variable is equal to 7
    IF me->mv_conditional = 7.
      ir_out->write( 'The variable is equal to 7' ).
    ELSE.
      ir_out->write( 'The variable is different from 7' ).
    ENDIF.
*     Assign a new value to the variable
    me->mv_conditional = 5.
*     Validate again
    IF me->mv_conditional = 7.
      ir_out->write( 'The variable is equal to 7' ).
    ELSE.
      ir_out->write( 'The variable is different from 7' ).
    ENDIF.
  ENDMETHOD.

  METHOD perform_case_endcase.
*     Declare and assign the variable
    me->mv_string = 'LOGALI'.

*     Validate the variable using CASE
    ir_out->write( me->get_case_result( me->mv_string ) ).

*     Assign new values and validate again
    me->mv_string = 'SAP'.
    ir_out->write( |{ me->get_case_result( me->mv_string ) }| ).
    me->mv_string = 'OTHER'.
    ir_out->write( |{ me->get_case_result( me->mv_string ) }| ).
  ENDMETHOD.

  METHOD get_case_result.
*     Method to return the result based on the input value
    CASE iv_string.
      WHEN 'LOGALI'.
        rv_result = 'Academy'.
      WHEN 'SAP'.
        rv_result = 'Enterprise software'.
      WHEN OTHERS.
        rv_result = 'Unknown'.
    ENDCASE.
  ENDMETHOD.

  METHOD perform_do_enddo.
*     Declare and initialize the variable
    me->mv_counter = 0.
*     Perform 10 iterations
    DO 10 TIMES.
      me->mv_counter = me->mv_counter + 1.
      ir_out->write( |{ me->mv_counter }| ).
    ENDDO.
  ENDMETHOD.

  METHOD perform_check.
*     Initialize the variable
    me->mv_counter = 0.
*     Perform iterations with CHECK
    DO 10 TIMES.
      me->mv_counter = me->mv_counter + 1.
      CHECK me->mv_counter LE 7.
      ir_out->write( |{ me->mv_counter }| ).
    ENDDO.
  ENDMETHOD.

  METHOD perform_switch.
*     Declare and assign the variable
    me->mv_string_2 = 'LOGALI'.
*     Validate the variable using SWITCH
    ir_out->write( SWITCH string( me->mv_string_2
      WHEN 'LOGALI' THEN 'SAP Academy'
      WHEN 'SAP' THEN 'Enterprise software'
      WHEN 'MOVISTAR' THEN 'Telephony'
      ELSE 'Unknown' ) ).
*     Assign new values and validate again
    me->mv_string_2 = 'SAP'.
    ir_out->write( SWITCH string( me->mv_string_2
      WHEN 'LOGALI' THEN 'SAP Academy'
      WHEN 'SAP' THEN 'Enterprise software'
      WHEN 'MOVISTAR' THEN 'Telephony'
      ELSE 'Unknown' ) ).
    me->mv_string_2 = 'MOVISTAR'.
    ir_out->write( SWITCH string( me->mv_string_2
      WHEN 'LOGALI' THEN 'SAP Academy'
      WHEN 'SAP' THEN 'Enterprise software'
      WHEN 'MOVISTAR' THEN 'Telephony'
      ELSE 'Unknown' ) ).
    me->mv_string_2 = 'OTHER'.
    ir_out->write( SWITCH string( me->mv_string_2
      WHEN 'LOGALI' THEN 'SAP Academy'
      WHEN 'SAP' THEN 'Enterprise software'
      WHEN 'MOVISTAR' THEN 'Telephony'
      ELSE 'Unknown' ) ).
  ENDMETHOD.

  METHOD perform_cond.
*     Declare and assign the variable
    me->mv_time = sy-timlo.
*     Identify the time format
    ir_out->write( COND string(
      WHEN me->mv_time LT '120000' THEN |{ me->mv_time } AM|
      WHEN me->mv_time GT '120000' THEN |{ me->mv_time } PM|
      WHEN me->mv_time EQ '120000' THEN |{ me->mv_time } High Noon|
    ) ).
  ENDMETHOD.

  METHOD perform_while_endwhile.
*     Declare and initialize the variable
    me->mv_counter_2 = 0.
*     Perform the WHILE loop
    WHILE me->mv_counter_2 LT 20.
      me->mv_counter_2 = me->mv_counter_2 + 1.
      IF me->mv_counter_2 = 10.
        CONTINUE.
      ENDIF.
      ir_out->write( |{ me->mv_counter_2 }| ).
    ENDWHILE.
  ENDMETHOD.

  METHOD perform_loop_endloop.
*     Declare the internal table and structure
    DATA: mt_employees TYPE TABLE OF zemp_logali,
          ms_employee  TYPE zemp_logali.
*     Query the ZEMP_LOGALI table
    SELECT * FROM zemp_logali INTO TABLE mt_employees.
*     Iterate over the internal table
    LOOP AT mt_employees INTO ms_employee WHERE ape2 = 'JIMENEZ'.
      ir_out->write( |{ ms_employee-email }| ).
    ENDLOOP.
  ENDMETHOD.

  METHOD perform_try_endtry.
*     Declare and initialize the variable
    me->mv_exception = 5.
    me->mv_counter = 5.
*     Perform the DO loop
    DO 5 TIMES.
      TRY.
          me->mv_counter = me->mv_counter - 1.
          me->mv_exception = me->mv_exception / me->mv_counter.
          ir_out->write( |{ me->mv_exception }| ).
        CATCH cx_sy_zerodivide INTO DATA(lv_exc).
          ir_out->write( | Error: { lv_exc->get_text(  ) }| ). " Division by zero
      ENDTRY.
    ENDDO.
  ENDMETHOD.
ENDCLASS.