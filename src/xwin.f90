module xwin
    use, intrinsic :: iso_c_binding
    use :: cairo
    use :: xlib
    implicit none (type, external)
    private

    type, public :: ctx_type
        type(c_ptr)          :: cairo            = c_null_ptr
        type(c_ptr)          :: surface          = c_null_ptr
        type(c_ptr)          :: display          = c_null_ptr
        integer              :: screen           = 0
        integer(kind=c_long) :: root             = 0
        integer(kind=c_long) :: window           = 0
        integer(kind=c_long) :: wm_delete_window = 0
    end type ctx_type

    type, public :: image_type
        type(c_ptr) :: surface = c_null_ptr
        integer     :: width   = 0
        integer     :: height  = 0
    end type image_type

    public :: xwin_create
    public :: xwin_destroy
    public :: xwin_load_png
    public :: xwin_set_png
contains
    subroutine xwin_create(ctx, width, height, title, stat)
        type(ctx_type),   intent(inout)         :: ctx
        integer,          intent(in)            :: width
        integer,          intent(in)            :: height
        character(len=*), intent(in)            :: title
        integer,          intent(out), optional :: stat
        integer                                 :: rc
        integer(kind=c_long)                    :: black, white
        type(x_size_hints)                      :: size_hints

        if (present(stat)) stat = -1

        ctx%display = x_open_display(c_null_char)
        if (.not. c_associated(ctx%display)) return

        ctx%screen = x_default_screen(ctx%display)
        ctx%root   = x_default_root_window(ctx%display)
        black      = x_black_pixel(ctx%display, ctx%screen)
        white      = x_white_pixel(ctx%display, ctx%screen)
        ctx%window = x_create_simple_window(ctx%display, ctx%root, 0, 0, width, height, 0, black, white)

        ctx%wm_delete_window = x_intern_atom(ctx%display, 'WM_DELETE_WINDOW' // c_null_char, .false._c_bool)
        rc = x_set_wm_protocols(ctx%display, ctx%window, ctx%wm_delete_window, 1)

        size_hints%flags      = ior(P_MIN_SIZE, P_MAX_SIZE)
        size_hints%min_width  = width
        size_hints%min_height = height
        size_hints%max_width  = width
        size_hints%max_height = height

        call x_set_wm_normal_hints(ctx%display, ctx%window, size_hints)

        call x_store_name(ctx%display, ctx%window, title // c_null_char)
        call x_select_input(ctx%display, ctx%window, ior(EXPOSURE_MASK, KEY_PRESS_MASK))
        call x_map_window(ctx%display, ctx%window)

        call x_clear_window(ctx%display, ctx%window)
        call x_sync(ctx%display, .false._c_bool)

        if (present(stat)) stat = 0
    end subroutine xwin_create

    subroutine xwin_destroy(ctx)
        type(ctx_type), intent(inout) :: ctx

        call cairo_destroy(ctx%cairo)
        call cairo_surface_destroy(ctx%surface)

        call x_destroy_window(ctx%display, ctx%window)
        call x_close_display(ctx%display)
    end subroutine xwin_destroy

    subroutine xwin_load_png(image, file_name, stat)
        type(image_type), intent(out)           :: image
        character(len=*), intent(in)            :: file_name
        integer,          intent(out), optional :: stat

        if (present(stat)) stat = -1

        image%surface = cairo_image_surface_create_from_png(file_name // c_null_char)
        if (cairo_surface_status(image%surface) /= CAIRO_STATUS_SUCCESS) return

        image%width  = cairo_image_surface_get_width(image%surface)
        image%height = cairo_image_surface_get_height(image%surface)

        if (image%width == 0 .or. image%height == 0) return
        if (present(stat)) stat = 0
    end subroutine xwin_load_png

    subroutine xwin_set_png(ctx, image, x, y)
        type(ctx_type),   intent(inout) :: ctx
        type(image_type), intent(inout) :: image
        integer,          intent(in)    :: x
        integer,          intent(in)    :: y

        ctx%surface = cairo_xlib_surface_create(ctx%display, &
                                                ctx%window, &
                                                x_default_visual(ctx%display, ctx%screen), &
                                                image%width, &
                                                image%height)
        ctx%cairo = cairo_create(ctx%surface)
        call cairo_set_source_surface(ctx%cairo, image%surface, dble(x), dble(y))
    end subroutine xwin_set_png
end module xwin
