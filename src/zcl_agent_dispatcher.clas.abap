CLASS zcl_agent_dispatcher DEFINITION
  PUBLIC
  CREATE PUBLIC.

  PUBLIC SECTION.
    INTERFACES if_http_extension.

    CLASS-METHODS refresh_cache.

    CLASS-METHODS get_registered_endpoints
      RETURNING
        VALUE(rt_endpoints) TYPE zagent_endpoints_tt.

  PRIVATE SECTION.
    TYPES: BEGIN OF ty_endpoint_cache,
             endpoint_path TYPE zdyn_endpoint,
             http_method   TYPE zdyn_http_method,
             class_name    TYPE seoclsname,
             method_name   TYPE seocpdname,
             auth_object   TYPE xuobject,
           END OF ty_endpoint_cache.

    TYPES tt_endpoint_cache TYPE HASHED TABLE OF ty_endpoint_cache
          WITH UNIQUE KEY endpoint_path http_method.

    CLASS-DATA gt_endpoint_cache TYPE tt_endpoint_cache.
    CLASS-DATA gv_cache_loaded   TYPE abap_bool.

    CLASS-METHODS set_cors_headers
      IMPORTING
        io_response TYPE REF TO if_http_response.

    CLASS-METHODS resolve_handler
      IMPORTING
        iv_path        TYPE string
        iv_method      TYPE string
      EXPORTING
        ev_class_name  TYPE seoclsname
        ev_method_name TYPE seocpdname
        ev_auth_object TYPE xuobject
        rv_found       TYPE abap_bool.

    CLASS-METHODS instantiate_and_call
      IMPORTING
        iv_class_name  TYPE seoclsname
        iv_method_name TYPE seocpdname
        iv_auth_object TYPE xuobject
        io_request     TYPE REF TO if_http_request
        io_response    TYPE REF TO if_http_response.

    CLASS-METHODS load_cache.

    CLASS-METHODS find_matching_endpoint
      IMPORTING
        iv_path        TYPE string
        iv_method      TYPE string
      EXPORTING
        ev_class_name  TYPE seoclsname
        ev_method_name TYPE seocpdname
        ev_auth_object TYPE xuobject
        rv_found       TYPE abap_bool.

ENDCLASS.



CLASS zcl_agent_dispatcher IMPLEMENTATION.

  METHOD if_http_extension~handle_request.
    DATA: lv_path        TYPE string,
          lv_method      TYPE string,
          lv_class_name  TYPE seoclsname,
          lv_method_name TYPE seocpdname,
          lv_found       TYPE abap_bool,
          lv_auth_object TYPE xuobject.

    set_cors_headers( server->response ).

    lv_method = server->request->get_method( ).

    IF lv_method = 'OPTIONS'.
      server->response->set_status( code = 200 reason = 'OK' ).
      RETURN.
    ENDIF.

    lv_path = server->request->get_header_field( '~path_info' ).

    IF gv_cache_loaded = abap_false.
      load_cache( ).
    ENDIF.

    resolve_handler(
        EXPORTING
          iv_path   = lv_path
          iv_method = lv_method
        IMPORTING
          ev_class_name  = lv_class_name
          ev_method_name = lv_method_name
          ev_auth_object = lv_auth_object
          rv_found       = lv_found ).

    IF lv_found IS NOT INITIAL.

      instantiate_and_call(
        iv_class_name  = lv_class_name
        iv_method_name = lv_method_name
        iv_auth_object = lv_auth_object
        io_request     = server->request
        io_response    = server->response ).

    ELSE.
      server->response->set_cdata(
        '{"status":"error","message":"Unknown endpoint"}' ).
      server->response->set_status( code = 404 reason = 'Not Found' ).
      server->response->set_content_type( 'application/json' ).
    ENDIF.
  ENDMETHOD.


  METHOD get_registered_endpoints.
    SELECT * FROM zagent_endpoints
      INTO TABLE rt_endpoints
      WHERE is_active = abap_true
      ORDER BY category endpoint_path.
  ENDMETHOD.


  METHOD refresh_cache.
    gv_cache_loaded = abap_false.
  ENDMETHOD.


  METHOD find_matching_endpoint.
    DATA: ls_cache     TYPE ty_endpoint_cache,
          lv_path_upper TYPE char100.

    rv_found = abap_false.
    lv_path_upper = iv_path.

    READ TABLE gt_endpoint_cache INTO ls_cache
      WITH KEY endpoint_path = lv_path_upper
               http_method   = iv_method.

    IF sy-subrc = 0.
      ev_class_name  = ls_cache-class_name.
      ev_method_name = ls_cache-method_name.
      ev_auth_object = ls_cache-auth_object.
      rv_found = abap_true.
      RETURN.
    ENDIF.

    LOOP AT gt_endpoint_cache INTO ls_cache
      WHERE http_method = iv_method.

      IF iv_path CS ls_cache-endpoint_path.
        ev_class_name  = ls_cache-class_name.
        ev_method_name = ls_cache-method_name.
        ev_auth_object = ls_cache-auth_object.
        rv_found = abap_true.
        RETURN.
      ENDIF.
    ENDLOOP.
  ENDMETHOD.


  METHOD instantiate_and_call.
    DATA: lo_handler  TYPE REF TO zcl_agent_handler_base,
          lx_error    TYPE REF TO cx_root,
          lv_error    TYPE string.

    TRY.
        CREATE OBJECT lo_handler TYPE (iv_class_name).

        lo_handler->zif_agent_handler~initialize(
          io_request  = io_request
          io_response = io_response ).

        IF iv_auth_object IS NOT INITIAL.
          IF lo_handler->zif_agent_handler~check_authorization(
               iv_auth_object ) = abap_false.
            io_response->set_cdata(
              '{"status":"error","message":"Not authorized"}' ).
            io_response->set_status( code = 403 reason = 'Forbidden' ).
            io_response->set_content_type( 'application/json' ).
            RETURN.
          ENDIF.
        ENDIF.

        CALL METHOD lo_handler->(iv_method_name).

      CATCH cx_sy_create_object_error INTO lx_error.
        lv_error = lx_error->get_text( ).
        io_response->set_cdata(
          '{"status":"error","message":"Handler class not found"}' ).
        io_response->set_status( code = 500 reason = 'Internal Server Error' ).
        io_response->set_content_type( 'application/json' ).

      CATCH cx_root INTO lx_error.
        lv_error = lx_error->get_text( ).
        REPLACE ALL OCCURRENCES OF '"' IN lv_error WITH '\"'.
        io_response->set_cdata(
          '{"status":"error","message":"Handler error"}' ).
        io_response->set_status( code = 500 reason = 'Internal Server Error' ).
        io_response->set_content_type( 'application/json' ).
    ENDTRY.
  ENDMETHOD.


  METHOD load_cache.
    DATA: lt_endpoints TYPE STANDARD TABLE OF zagent_endpoints,
          ls_endpoint  TYPE zagent_endpoints,
          ls_cache     TYPE ty_endpoint_cache.

    CLEAR gt_endpoint_cache.

    SELECT * FROM zagent_endpoints
      INTO TABLE lt_endpoints
      WHERE is_active = abap_true.

    LOOP AT lt_endpoints INTO ls_endpoint.
      CLEAR ls_cache.
      ls_cache-endpoint_path = ls_endpoint-endpoint_path.
      ls_cache-http_method   = ls_endpoint-http_method.
      ls_cache-class_name    = ls_endpoint-class_name.
      ls_cache-method_name   = ls_endpoint-method_name.
      ls_cache-auth_object   = ls_endpoint-auth_object.
      INSERT ls_cache INTO TABLE gt_endpoint_cache.
    ENDLOOP.

    gv_cache_loaded = abap_true.
  ENDMETHOD.


  METHOD resolve_handler.
    find_matching_endpoint(
       EXPORTING
         iv_path   = iv_path
         iv_method = iv_method
       IMPORTING
         ev_class_name  = ev_class_name
         ev_method_name = ev_method_name
         ev_auth_object = ev_auth_object
         rv_found       = rv_found ).
  ENDMETHOD.


  METHOD set_cors_headers.
    io_response->set_header_field(
      name  = 'Access-Control-Allow-Origin'
      value = '*' ).
    io_response->set_header_field(
      name  = 'Access-Control-Allow-Methods'
      value = 'GET,POST,PUT,DELETE,PATCH,OPTIONS' ).
    io_response->set_header_field(
      name  = 'Access-Control-Allow-Headers'
      value = 'Content-Type,Authorization,X-Requested-With' ).
  ENDMETHOD.

ENDCLASS.
