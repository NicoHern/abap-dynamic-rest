CLASS zcl_agent_handler_base DEFINITION
  PUBLIC
  ABSTRACT
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES zif_agent_handler.

    ALIASES mo_request  FOR zif_agent_handler~mo_request.
    ALIASES mo_response FOR zif_agent_handler~mo_response.
    ALIASES mv_body     FOR zif_agent_handler~mv_body.

    METHODS constructor.

    METHODS parse_json_param
      IMPORTING
        iv_param_name   TYPE string
        iv_to_upper     TYPE abap_bool DEFAULT abap_false
      RETURNING
        VALUE(rv_value) TYPE string.

    METHODS parse_json_int
      IMPORTING
        iv_param_name   TYPE string
        iv_default      TYPE i DEFAULT 0
      RETURNING
        VALUE(rv_value) TYPE i.

    METHODS parse_json_bool
      IMPORTING
        iv_param_name   TYPE string
        iv_default      TYPE abap_bool DEFAULT ''
      RETURNING
        VALUE(rv_value) TYPE abap_bool.

    METHODS escape_json_string
      IMPORTING
        iv_input         TYPE string
      RETURNING
        VALUE(rv_output) TYPE string.

    METHODS send_success
      IMPORTING
        iv_json TYPE string
        iv_code TYPE i DEFAULT 200.

    METHODS send_error
      IMPORTING
        iv_message TYPE string
        iv_code    TYPE i DEFAULT 400.

    METHODS send_json
      IMPORTING
        iv_json   TYPE string
        iv_code   TYPE i DEFAULT 200
        iv_reason TYPE string DEFAULT 'OK'.

  PROTECTED SECTION.
    DATA mv_initialized TYPE abap_bool.

  PRIVATE SECTION.
    METHODS get_status_reason
      IMPORTING
        iv_code          TYPE i
      RETURNING
        VALUE(rv_reason) TYPE string.

ENDCLASS.



CLASS zcl_agent_handler_base IMPLEMENTATION.

  METHOD constructor.
    mv_initialized = abap_false.
  ENDMETHOD.


  METHOD zif_agent_handler~initialize.
    mo_request  = io_request.
    mo_response = io_response.

    IF mo_request IS BOUND.
      mv_body = mo_request->get_cdata( ).
    ENDIF.

    mv_initialized = abap_true.
  ENDMETHOD.


  METHOD zif_agent_handler~get_supported_methods.
    APPEND 'POST' TO rt_methods.
  ENDMETHOD.


  METHOD zif_agent_handler~check_authorization.
    rv_authorized = abap_true.

    IF iv_auth_object IS NOT INITIAL.
      AUTHORITY-CHECK OBJECT iv_auth_object
        ID 'ACTVT' FIELD '03'.
      IF sy-subrc = 0.
        rv_authorized = abap_true.
      ENDIF.
    ENDIF.
  ENDMETHOD.


  METHOD parse_json_param.
    DATA: lv_pattern TYPE string.

    CLEAR rv_value.

    CONCATENATE '"' iv_param_name '"\s*:\s*"([^"]*)"' INTO lv_pattern.

    FIND REGEX lv_pattern IN mv_body SUBMATCHES rv_value.

    IF iv_to_upper = abap_true AND rv_value IS NOT INITIAL.
      TRANSLATE rv_value TO UPPER CASE.
    ENDIF.
  ENDMETHOD.


  METHOD parse_json_int.
    DATA: lv_pattern TYPE string,
          lv_value   TYPE string.

    rv_value = iv_default.

    CONCATENATE '"' iv_param_name '"\s*:\s*(\d+)' INTO lv_pattern.

    FIND REGEX lv_pattern IN mv_body SUBMATCHES lv_value.

    IF lv_value IS NOT INITIAL.
      rv_value = lv_value.
    ENDIF.
  ENDMETHOD.


  METHOD parse_json_bool.
    DATA: lv_pattern TYPE string,
          lv_value   TYPE string.

    rv_value = iv_default.

    CONCATENATE '"' iv_param_name '"\s*:\s*(true|false)' INTO lv_pattern.

    FIND REGEX lv_pattern IN mv_body SUBMATCHES lv_value.

    IF lv_value = 'true'.
      rv_value = abap_true.
    ELSEIF lv_value = 'false'.
      rv_value = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD escape_json_string.
    rv_output = iv_input.

    REPLACE ALL OCCURRENCES OF '\' IN rv_output WITH '\\'.
    REPLACE ALL OCCURRENCES OF '"' IN rv_output WITH '\"'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>cr_lf IN rv_output WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>newline IN rv_output WITH '\n'.
    REPLACE ALL OCCURRENCES OF cl_abap_char_utilities=>horizontal_tab IN rv_output WITH '\t'.
  ENDMETHOD.


  METHOD get_status_reason.
    CASE iv_code.
      WHEN 200. rv_reason = 'OK'.
      WHEN 201. rv_reason = 'Created'.
      WHEN 400. rv_reason = 'Bad Request'.
      WHEN 401. rv_reason = 'Unauthorized'.
      WHEN 403. rv_reason = 'Forbidden'.
      WHEN 404. rv_reason = 'Not Found'.
      WHEN 500. rv_reason = 'Internal Server Error'.
      WHEN OTHERS. rv_reason = 'Unknown'.
    ENDCASE.
  ENDMETHOD.


  METHOD send_error.
    DATA: lv_response TYPE string,
          lv_escaped  TYPE string,
          lv_reason   TYPE string.

    lv_escaped = escape_json_string( iv_message ).
    CONCATENATE '{"status":"error","message":"' lv_escaped '"}' INTO lv_response.

    lv_reason = get_status_reason( iv_code ).
    send_json(
      iv_json   = lv_response
      iv_code   = iv_code
      iv_reason = lv_reason ).
  ENDMETHOD.


  METHOD send_json.
    CHECK mo_response IS BOUND.

    mo_response->set_cdata( iv_json ).
    mo_response->set_status( code = iv_code reason = iv_reason ).
    mo_response->set_content_type( 'application/json' ).
  ENDMETHOD.


  METHOD send_success.
    DATA: lv_response TYPE string,
          lv_reason   TYPE string.

    CONCATENATE '{"status":"success",' iv_json '}' INTO lv_response.

    lv_reason = get_status_reason( iv_code ).
    send_json(
      iv_json   = lv_response
      iv_code   = iv_code
      iv_reason = lv_reason ).
  ENDMETHOD.

ENDCLASS.
