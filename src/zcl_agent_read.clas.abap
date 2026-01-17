CLASS zcl_agent_read DEFINITION
  PUBLIC
  FINAL
  CREATE PUBLIC
  INHERITING FROM zcl_agent_handler_base.

  PUBLIC SECTION.
    "! Read table structure/metadata
    METHODS handle_read_table_structure.

    "! Read table data with optional filters
    METHODS handle_read_table_data.

    "! Read table data with pagination (streaming)
    METHODS handle_stream_table_data.

    "! Read domain fixed values
    METHODS handle_read_domain_values.

    "! Check if a type/table exists
    METHODS handle_check_type_exists.

protected section.
  PRIVATE SECTION.
    TYPES: BEGIN OF ty_dd03p,
             fieldname TYPE dd03p-fieldname,
             keyflag   TYPE dd03p-keyflag,
             rollname  TYPE dd03p-rollname,
             datatype  TYPE dd03p-datatype,
             leng      TYPE dd03p-leng,
             decimals  TYPE dd03p-decimals,
             ddtext    TYPE dd03p-ddtext,
           END OF ty_dd03p.

    TYPES tt_dd03p TYPE STANDARD TABLE OF ty_dd03p WITH DEFAULT KEY.

    METHODS check_table_auth
      RETURNING
        VALUE(rv_authorized) TYPE abap_bool.

    METHODS build_metadata_json
      IMPORTING
        it_dd03p      TYPE tt_dd03p
      RETURNING
        VALUE(rv_json) TYPE string.

ENDCLASS.



CLASS ZCL_AGENT_READ IMPLEMENTATION.


  METHOD handle_read_table_structure.
    DATA: lv_table_name TYPE dd02l-tabname,
          lt_dd03p      TYPE tt_dd03p,
          ls_dd03p      TYPE ty_dd03p,
          lt_dd03p_full TYPE STANDARD TABLE OF dd03p,
          ls_dd03p_full TYPE dd03p,
          lv_response   TYPE string.

    " Get table name from request
    lv_table_name = parse_json_param( iv_param_name = 'table_name' iv_to_upper = abap_true ).

    IF lv_table_name IS INITIAL.
      send_error( iv_message = 'Missing parameter: table_name' iv_code = 400 ).
      RETURN.
    ENDIF.

    " Authorization check
    IF check_table_auth( ) = abap_false.
      send_error( iv_message = 'Not authorized for table access' iv_code = 403 ).
      RETURN.
    ENDIF.

    " Get table fields
    CALL FUNCTION 'DDIF_TABL_GET'
      EXPORTING
        name          = lv_table_name
        langu         = sy-langu
      TABLES
        dd03p_tab     = lt_dd03p_full
      EXCEPTIONS
        illegal_input = 1
        OTHERS        = 2.

    IF sy-subrc <> 0.
      send_error( iv_message = 'Table not found' iv_code = 404 ).
      RETURN.
    ENDIF.

    " Map to simplified structure
    LOOP AT lt_dd03p_full INTO ls_dd03p_full.
      CLEAR ls_dd03p.
      ls_dd03p-fieldname = ls_dd03p_full-fieldname.
      ls_dd03p-keyflag   = ls_dd03p_full-keyflag.
      ls_dd03p-rollname  = ls_dd03p_full-rollname.
      ls_dd03p-datatype  = ls_dd03p_full-datatype.
      ls_dd03p-leng      = ls_dd03p_full-leng.
      ls_dd03p-decimals  = ls_dd03p_full-decimals.
      ls_dd03p-ddtext    = ls_dd03p_full-ddtext.
      APPEND ls_dd03p TO lt_dd03p.
    ENDLOOP.

    " Build JSON response
    lv_response = build_metadata_json( lt_dd03p ).
    send_success( lv_response ).
  ENDMETHOD.


  METHOD handle_read_table_data.
    DATA: lv_table_name  TYPE dd02l-tabname,
          lv_max_rows    TYPE i,
          lv_where       TYPE string,
          lr_data        TYPE REF TO data,
          lv_response    TYPE string,
          lv_rows_str    TYPE string,
          lv_first       TYPE abap_bool,
          lv_first_col   TYPE abap_bool,
          lv_fieldname   TYPE string,
          lv_field_val   TYPE string,
          lt_dd03p       TYPE STANDARD TABLE OF dd03p,
          ls_dd03p       TYPE dd03p,
          lx_error       TYPE REF TO cx_root,
          lv_lines       TYPE i.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_data>  TYPE any,
                   <lv_val>   TYPE any.

    " Get parameters
    lv_table_name = parse_json_param( iv_param_name = 'table_name' iv_to_upper = abap_true ).
    lv_max_rows   = parse_json_int( iv_param_name = 'max_rows' iv_default = 100 ).
    lv_where      = parse_json_param( iv_param_name = 'where' ).

    IF lv_table_name IS INITIAL.
      send_error( iv_message = 'Missing parameter: table_name' iv_code = 400 ).
      RETURN.
    ENDIF.

    " Limit max rows for safety
    IF lv_max_rows > 1000.
      lv_max_rows = 1000.
    ENDIF.

    " Authorization check
    IF check_table_auth( ) = abap_false.
      send_error( iv_message = 'Not authorized for table access' iv_code = 403 ).
      RETURN.
    ENDIF.

    TRY.
        " Create dynamic internal table
        CREATE DATA lr_data TYPE TABLE OF (lv_table_name).
        ASSIGN lr_data->* TO <lt_data>.

        " Get field catalog for output
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = lv_table_name
            langu         = sy-langu
          TABLES
            dd03p_tab     = lt_dd03p
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.

        " Execute dynamic select
        IF lv_where IS NOT INITIAL.
          SELECT * FROM (lv_table_name)
            INTO TABLE <lt_data>
            UP TO lv_max_rows ROWS
            WHERE (lv_where).
        ELSE.
          SELECT * FROM (lv_table_name)
            INTO TABLE <lt_data>
            UP TO lv_max_rows ROWS.
        ENDIF.

        " Build JSON response
        DESCRIBE TABLE <lt_data> LINES lv_lines.
        lv_rows_str = lv_lines.
        CONDENSE lv_rows_str NO-GAPS.

        CONCATENATE '"row_count":' lv_rows_str ',"data":[' INTO lv_response.

        lv_first = abap_true.
        LOOP AT <lt_data> ASSIGNING <ls_data>.
          IF lv_first = abap_false.
            CONCATENATE lv_response ',' INTO lv_response.
          ENDIF.
          lv_first = abap_false.

          CONCATENATE lv_response '{' INTO lv_response.
          lv_first_col = abap_true.

          LOOP AT lt_dd03p INTO ls_dd03p.
            " Skip includes
            IF ls_dd03p-fieldname CP '.INCLUDE' OR ls_dd03p-fieldname CP '.INCLU*'.
              CONTINUE.
            ENDIF.

            IF lv_first_col = abap_false.
              CONCATENATE lv_response ',' INTO lv_response.
            ENDIF.
            lv_first_col = abap_false.

            lv_fieldname = ls_dd03p-fieldname.
            ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_data> TO <lv_val>.

            IF sy-subrc = 0.
              CLEAR lv_field_val.
              MOVE <lv_val> TO lv_field_val.
              CONDENSE lv_field_val.
              lv_field_val = escape_json_string( lv_field_val ).
              CONCATENATE lv_response '"' lv_fieldname '":"' lv_field_val '"' INTO lv_response.
            ELSE.
              CONCATENATE lv_response '"' lv_fieldname '":""' INTO lv_response.
            ENDIF.
          ENDLOOP.

          CONCATENATE lv_response '}' INTO lv_response.
        ENDLOOP.

        CONCATENATE lv_response ']' INTO lv_response.
        send_success( lv_response ).

      CATCH cx_root INTO lx_error.
        DATA: lv_err_msg TYPE string.
        lv_err_msg = lx_error->get_text( ).
        send_error( iv_message = lv_err_msg iv_code = 500 ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_stream_table_data.
    DATA: lv_table_name    TYPE dd02l-tabname,
          lv_package_size  TYPE i,
          lv_offset        TYPE i,
          lv_cursor        TYPE cursor,
          lr_data          TYPE REF TO data,
          lv_response      TYPE string,
          lv_rows_str      TYPE string,
          lv_rows_fetched  TYPE i,
          lv_first         TYPE abap_bool,
          lv_first_col     TYPE abap_bool,
          lv_fieldname     TYPE string,
          lv_field_val     TYPE string,
          lt_dd03p         TYPE STANDARD TABLE OF dd03p,
          ls_dd03p         TYPE dd03p,
          lx_error         TYPE REF TO cx_root.

    FIELD-SYMBOLS: <lt_data>  TYPE STANDARD TABLE,
                   <ls_data>  TYPE any,
                   <lv_val>   TYPE any.

    " Get parameters
    lv_table_name   = parse_json_param( iv_param_name = 'table_name' iv_to_upper = abap_true ).
    lv_package_size = parse_json_int( iv_param_name = 'package_size' iv_default = 100 ).
    lv_offset       = parse_json_int( iv_param_name = 'offset' iv_default = 0 ).

    IF lv_table_name IS INITIAL.
      send_error( iv_message = 'Missing parameter: table_name' iv_code = 400 ).
      RETURN.
    ENDIF.

    " Limit package size
    IF lv_package_size > 500.
      lv_package_size = 500.
    ENDIF.

    " Authorization check
    IF check_table_auth( ) = abap_false.
      send_error( iv_message = 'Not authorized for table access' iv_code = 403 ).
      RETURN.
    ENDIF.

    TRY.
        " Create dynamic internal table
        CREATE DATA lr_data TYPE TABLE OF (lv_table_name).
        ASSIGN lr_data->* TO <lt_data>.

        " Get field catalog
        CALL FUNCTION 'DDIF_TABL_GET'
          EXPORTING
            name          = lv_table_name
            langu         = sy-langu
          TABLES
            dd03p_tab     = lt_dd03p
          EXCEPTIONS
            illegal_input = 1
            OTHERS        = 2.

        " Open cursor
        OPEN CURSOR lv_cursor FOR
          SELECT * FROM (lv_table_name).

        " Skip rows for offset
        IF lv_offset > 0.
          DATA: lr_skip TYPE REF TO data.
          FIELD-SYMBOLS: <lt_skip> TYPE STANDARD TABLE.

          CREATE DATA lr_skip TYPE TABLE OF (lv_table_name).
          ASSIGN lr_skip->* TO <lt_skip>.

          FETCH NEXT CURSOR lv_cursor
            INTO TABLE <lt_skip>
            PACKAGE SIZE lv_offset.

          CLEAR <lt_skip>.
          FREE lr_skip.
        ENDIF.

        " Fetch actual data
        FETCH NEXT CURSOR lv_cursor
          INTO TABLE <lt_data>
          PACKAGE SIZE lv_package_size.

        CLOSE CURSOR lv_cursor.

        " Build response
        DESCRIBE TABLE <lt_data> LINES lv_rows_fetched.
        lv_rows_str = lv_rows_fetched.
        CONDENSE lv_rows_str NO-GAPS.

        CONCATENATE '"rows_in_chunk":' lv_rows_str ',"data":[' INTO lv_response.

        lv_first = abap_true.
        LOOP AT <lt_data> ASSIGNING <ls_data>.
          IF lv_first = abap_false.
            CONCATENATE lv_response ',' INTO lv_response.
          ENDIF.
          lv_first = abap_false.

          CONCATENATE lv_response '{' INTO lv_response.
          lv_first_col = abap_true.

          LOOP AT lt_dd03p INTO ls_dd03p.
            IF ls_dd03p-fieldname CP '.INCLUDE' OR ls_dd03p-fieldname CP '.INCLU*'.
              CONTINUE.
            ENDIF.

            IF lv_first_col = abap_false.
              CONCATENATE lv_response ',' INTO lv_response.
            ENDIF.
            lv_first_col = abap_false.

            lv_fieldname = ls_dd03p-fieldname.
            ASSIGN COMPONENT lv_fieldname OF STRUCTURE <ls_data> TO <lv_val>.

            IF sy-subrc = 0.
              CLEAR lv_field_val.
              MOVE <lv_val> TO lv_field_val.
              CONDENSE lv_field_val.
              lv_field_val = escape_json_string( lv_field_val ).
              CONCATENATE lv_response '"' lv_fieldname '":"' lv_field_val '"' INTO lv_response.
            ELSE.
              CONCATENATE lv_response '"' lv_fieldname '":""' INTO lv_response.
            ENDIF.
          ENDLOOP.

          CONCATENATE lv_response '}' INTO lv_response.
        ENDLOOP.

        CONCATENATE lv_response ']' INTO lv_response.
        send_success( lv_response ).

      CATCH cx_root INTO lx_error.
        TRY.
            CLOSE CURSOR lv_cursor.
          CATCH cx_root.
        ENDTRY.
        DATA: lv_error TYPE string.
        lv_error = lx_error->get_text( ).
        send_error( iv_message = lv_error iv_code = 500 ).
    ENDTRY.
  ENDMETHOD.


  METHOD handle_read_domain_values.
    DATA: lv_domain    TYPE dd01l-domname,
          lt_dd07v     TYPE STANDARD TABLE OF dd07v,
          ls_dd07v     TYPE dd07v,
          lv_response  TYPE string,
          lv_first     TYPE abap_bool,
          lv_value     TYPE string,
          lv_text      TYPE string.

    lv_domain = parse_json_param( iv_param_name = 'domain' iv_to_upper = abap_true ).

    IF lv_domain IS INITIAL.
      send_error( iv_message = 'Missing parameter: domain' iv_code = 400 ).
      RETURN.
    ENDIF.

    CALL FUNCTION 'DD_DOMVALUES_GET'
      EXPORTING
        domname        = lv_domain
        text           = abap_true
        langu          = sy-langu
      TABLES
        dd07v_tab      = lt_dd07v
      EXCEPTIONS
        wrong_textflag = 1
        OTHERS         = 2.

    IF sy-subrc <> 0.
      send_error( iv_message = 'Domain not found or has no fixed values' iv_code = 404 ).
      RETURN.
    ENDIF.

    lv_response = '"values":['.
    lv_first = abap_true.

    LOOP AT lt_dd07v INTO ls_dd07v.
      IF lv_first = abap_false.
        CONCATENATE lv_response ',' INTO lv_response.
      ENDIF.
      lv_first = abap_false.

      lv_value = escape_json_string( CONV string( ls_dd07v-domvalue_l ) ).
      lv_text  = escape_json_string( CONV string( ls_dd07v-ddtext ) ).

      CONCATENATE lv_response
        '{"value":"' lv_value '","text":"' lv_text '"}'
        INTO lv_response.
    ENDLOOP.

    CONCATENATE lv_response ']' INTO lv_response.
    send_success( lv_response ).
  ENDMETHOD.


  METHOD handle_check_type_exists.
    DATA: lv_type_name TYPE dd02l-tabname,
          lv_exists    TYPE abap_bool,
          lv_response  TYPE string,
          lv_tabclass  TYPE dd02l-tabclass.

    lv_type_name = parse_json_param( iv_param_name = 'type_name' iv_to_upper = abap_true ).

    IF lv_type_name IS INITIAL.
      send_error( iv_message = 'Missing parameter: type_name' iv_code = 400 ).
      RETURN.
    ENDIF.

    " Check if table/structure exists
    SELECT SINGLE tabclass FROM dd02l
      INTO lv_tabclass
      WHERE tabname = lv_type_name
        AND as4local = 'A'.

    IF sy-subrc = 0.
      lv_exists = abap_true.
    ELSE.
      lv_exists = abap_false.
    ENDIF.

    IF lv_exists = abap_true.
      CONCATENATE '"exists":true,"type_class":"' lv_tabclass '"' INTO lv_response.
    ELSE.
      lv_response = '"exists":false'.
    ENDIF.

    send_success( lv_response ).
  ENDMETHOD.


  METHOD check_table_auth.
    AUTHORITY-CHECK OBJECT 'S_TABU_DIS'
      ID 'DICBERCLS' DUMMY
      ID 'ACTVT' FIELD '03'.

    IF sy-subrc = 0.
      rv_authorized = abap_true.
    ELSE.
      rv_authorized = abap_false.
    ENDIF.
  ENDMETHOD.


  METHOD build_metadata_json.
    DATA: ls_dd03p       TYPE ty_dd03p,
          lv_first       TYPE abap_bool,
          lv_fieldname   TYPE string,
          lv_datatype    TYPE string,
          lv_leng        TYPE string,
          lv_decimals    TYPE string,
          lv_rollname    TYPE string,
          lv_fieldtext   TYPE string,
          lv_keyflag     TYPE string.

    rv_json = '"fields":['.
    lv_first = abap_true.

    LOOP AT it_dd03p INTO ls_dd03p.
      " Skip includes
      IF ls_dd03p-fieldname CP '.INCLUDE' OR ls_dd03p-fieldname CP '.INCLU*'.
        CONTINUE.
      ENDIF.

      IF lv_first = abap_false.
        CONCATENATE rv_json ',' INTO rv_json.
      ENDIF.
      lv_first = abap_false.

      lv_fieldname = escape_json_string( CONV string( ls_dd03p-fieldname ) ).
      lv_datatype  = ls_dd03p-datatype.

      lv_leng = ls_dd03p-leng.
      CONDENSE lv_leng NO-GAPS.

      lv_decimals = ls_dd03p-decimals.
      CONDENSE lv_decimals NO-GAPS.

      lv_rollname  = escape_json_string( CONV string( ls_dd03p-rollname ) ).
      lv_fieldtext = escape_json_string( CONV string( ls_dd03p-ddtext ) ).

      IF ls_dd03p-keyflag = 'X'.
        lv_keyflag = 'true'.
      ELSE.
        lv_keyflag = 'false'.
      ENDIF.

      CONCATENATE rv_json
        '{"name":"' lv_fieldname
        '","datatype":"' lv_datatype
        '","length":' lv_leng
        ',"decimals":' lv_decimals
        ',"rollname":"' lv_rollname
        '","description":"' lv_fieldtext
        '","is_key":' lv_keyflag '}'
        INTO rv_json.
    ENDLOOP.

    CONCATENATE rv_json ']' INTO rv_json.
  ENDMETHOD.
ENDCLASS.
