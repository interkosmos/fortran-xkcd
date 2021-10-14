module cairo
    use, intrinsic :: iso_c_binding
    implicit none
    private

    integer, public, parameter :: CAIRO_STATUS_SUCCESS = 0

    public :: cairo_create
    public :: cairo_destroy
    public :: cairo_image_surface_create_from_png
    public :: cairo_image_surface_get_height
    public :: cairo_image_surface_get_width
    public :: cairo_paint
    public :: cairo_set_source_surface
    public :: cairo_surface_destroy
    public :: cairo_surface_status
    public :: cairo_xlib_surface_create

    interface
        ! cairo_t * cairo_create(cairo_surface_t *target)
        function cairo_create(target) bind(c, name='cairo_create')
            import :: c_ptr
            type(c_ptr), intent(in), value :: target
            type(c_ptr)                    :: cairo_create
        end function cairo_create

        ! cairo_surface_t * cairo_image_surface_create_from_png(const char *filename)
        function cairo_image_surface_create_from_png(filename) bind(c, name='cairo_image_surface_create_from_png')
            import :: c_char, c_ptr
            character(kind=c_char), intent(in) :: filename
            type(c_ptr)                        :: cairo_image_surface_create_from_png
        end function cairo_image_surface_create_from_png

        ! int cairo_image_surface_get_height(cairo_surface_t *surface)
        function cairo_image_surface_get_height(surface) bind(c, name='cairo_image_surface_get_height')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: surface
            integer(kind=c_int)            :: cairo_image_surface_get_height
        end function cairo_image_surface_get_height

        ! int cairo_image_surface_get_width(cairo_surface_t *surface)
        function cairo_image_surface_get_width(surface) bind(c, name='cairo_image_surface_get_width')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: surface
            integer(kind=c_int)            :: cairo_image_surface_get_width
        end function cairo_image_surface_get_width

        ! cairo_status_t cairo_surface_status(cairo_surface_t *surface);
        function cairo_surface_status(surface) bind(c, name='cairo_surface_status')
            import :: c_int, c_ptr
            type(c_ptr), intent(in), value :: surface
            integer(kind=c_int)            :: cairo_surface_status
        end function cairo_surface_status

        ! cairo_surface_t * cairo_xlib_surface_create(Display *dpy, Drawable drawable, Visual *visual, int width, int height)
        function cairo_xlib_surface_create(dpy, drawable, visual, width, height) bind(c, name='cairo_xlib_surface_create')
            import :: c_int, c_long, c_ptr
            type(c_ptr),          intent(in), value :: dpy
            integer(kind=c_long), intent(in), value :: drawable
            type(c_ptr),          intent(in), value :: visual
            integer(kind=c_int),  intent(in), value :: width
            integer(kind=c_int),  intent(in), value :: height
            type(c_ptr)                             :: cairo_xlib_surface_create
        end function cairo_xlib_surface_create

        ! void cairo_destroy(cairo_t *cr)
        subroutine cairo_destroy(cr) bind(c, name='cairo_destroy')
            import :: c_ptr
            type(c_ptr), intent(in), value :: cr
        end subroutine cairo_destroy

        ! void cairo_paint(cairo_t *cr)
        subroutine cairo_paint(cr) bind(c, name='cairo_paint')
            import :: c_ptr
            type(c_ptr), intent(in), value :: cr
        end subroutine cairo_paint

        ! void cairo_surface_destroy(cairo_surface_t *surface)
        subroutine cairo_surface_destroy(surface) bind(c, name='cairo_surface_destroy')
            import :: c_ptr
            type(c_ptr), intent(in), value :: surface
        end subroutine cairo_surface_destroy

        ! void cairo_set_source_surface(cairo_t *cr, cairo_surface_t *surface, double x, double y)
        subroutine cairo_set_source_surface(cr, surface, x, y) bind(c, name='cairo_set_source_surface')
            import :: c_double, c_ptr
            type(c_ptr),         intent(in), value :: cr
            type(c_ptr),         intent(in), value :: surface
            real(kind=c_double), intent(in), value :: x
            real(kind=c_double), intent(in), value :: y
        end subroutine cairo_set_source_surface
    end interface
end module cairo

module xkcd
    use, intrinsic :: iso_c_binding
    use :: curl
    implicit none
    private

    character(len=*), parameter :: API_BASE = 'https://xkcd.com/'
    character(len=*), parameter :: API_JSON = 'info.0.json'

    type, public :: xkcd_data_type
        character(len=:), allocatable :: json
        character(len=:), allocatable :: month
        integer                       :: num
        character(len=:), allocatable :: link
        character(len=:), allocatable :: year
        character(len=:), allocatable :: news
        character(len=:), allocatable :: safe_title
        character(len=:), allocatable :: transcript
        character(len=:), allocatable :: alt
        character(len=:), allocatable :: img
        character(len=:), allocatable :: title
        character(len=:), allocatable :: day
    end type xkcd_data_type

    public  :: xkcd_fetch_json
    public  :: xkcd_fetch_png

    private :: xkcd_json_callback
    private :: xkcd_png_callback
contains
    function xkcd_json_callback(ptr, size, nmemb, client_data) bind(c)
        type(c_ptr),            intent(in), value :: ptr
        integer(kind=c_size_t), intent(in), value :: size
        integer(kind=c_size_t), intent(in), value :: nmemb
        type(c_ptr),            intent(in), value :: client_data
        integer(kind=c_size_t)                    :: xkcd_json_callback
        character(len=:), allocatable             :: chunk
        type(xkcd_data_type), pointer             :: xkcd_data

        xkcd_json_callback = int(0, kind=c_size_t)

        if (.not. c_associated(ptr)) return
        if (.not. c_associated(client_data)) return

        call c_f_pointer(client_data, xkcd_data)
        if (.not. associated(xkcd_data)) return
        call c_f_str_ptr(ptr, chunk, nmemb)

        if (.not. allocated(xkcd_data%json)) xkcd_data%json = ''
        xkcd_data%json = xkcd_data%json // chunk

        xkcd_json_callback = nmemb
    end function xkcd_json_callback

    function xkcd_png_callback(ptr, size, nmemb, client_data) bind(c)
        type(c_ptr),            intent(in), value :: ptr
        integer(kind=c_size_t), intent(in), value :: size
        integer(kind=c_size_t), intent(in), value :: nmemb
        type(c_ptr),            intent(in), value :: client_data
        integer(kind=c_size_t)                    :: xkcd_png_callback
        character(len=:), allocatable             :: chunk
        integer, pointer                          :: file_unit
        integer                                   :: stat

        xkcd_png_callback = int(0, kind=c_size_t)

        if (.not. c_associated(ptr)) return
        if (.not. c_associated(client_data)) return

        call c_f_pointer(client_data, file_unit)
        if (.not. associated(file_unit)) return

        inquire (unit=file_unit, iostat=stat)
        if (stat /= 0) return

        call c_f_str_ptr(ptr, chunk, nmemb)

        write (file_unit, iostat=stat) chunk
        if (stat /= 0) return

        xkcd_png_callback = nmemb
    end function xkcd_png_callback

    subroutine xkcd_fetch_json(num, xkcd_data, stat)
        use :: json_module
        integer,                      intent(in)            :: num
        type(xkcd_data_type), target, intent(out)           :: xkcd_data
        integer,                      intent(out), optional :: stat
        character(len=72) :: url
        integer           :: rc
        logical           :: found
        type(c_ptr)       :: curl_ptr
        type(json_file)   :: json

        if (present(stat)) stat = -1

        if (num > 0) then
            write (url, '(a, i0, "/", a)', iostat=rc) API_BASE, num, API_JSON
        else
            write (url, '(2a)', iostat=rc) API_BASE, API_JSON
        end if

        if (rc /= 0) return

        curl_ptr = curl_easy_init()
        if (.not. c_associated(curl_ptr)) return

        rc = curl_easy_setopt(curl_ptr, CURLOPT_DEFAULT_PROTOCOL, 'https' // c_null_char)
        rc = curl_easy_setopt(curl_ptr, CURLOPT_URL,              trim(url) // c_null_char)
        rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION,   int( 1, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT,          int(10, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL,         int( 1, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT,   int(10, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION,    c_funloc(xkcd_json_callback))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA,        c_loc(xkcd_data))

        rc = curl_easy_perform(curl_ptr)
        call curl_easy_cleanup(curl_ptr)

        if (rc /= CURLE_OK) return
        if (.not. allocated(xkcd_data%json)) return

        call json%initialize()
        call json%deserialize(xkcd_data%json)

        call json%get('month',      xkcd_data%month,      found); if (.not. found) return
        call json%get('num',        xkcd_data%num,        found); if (.not. found) return
        call json%get('link',       xkcd_data%link,       found); if (.not. found) return
        call json%get('year',       xkcd_data%year,       found); if (.not. found) return
        call json%get('news',       xkcd_data%news,       found); if (.not. found) return
        call json%get('safe_title', xkcd_data%safe_title, found); if (.not. found) return
        call json%get('transcript', xkcd_data%transcript, found); if (.not. found) return
        call json%get('alt',        xkcd_data%alt,        found); if (.not. found) return
        call json%get('img',        xkcd_data%img,        found); if (.not. found) return
        call json%get('title',      xkcd_data%title,      found); if (.not. found) return
        call json%get('day',        xkcd_data%day,        found); if (.not. found) return

        call json%destroy()

        if (present(stat)) stat = 0
    end subroutine xkcd_fetch_json

    subroutine xkcd_fetch_png(url, file_path, stat)
        character(len=*), intent(in)            :: url
        character(len=*), intent(in)            :: file_path
        integer,          intent(out), optional :: stat
        integer, target                         :: file_unit
        integer                                 :: rc
        type(c_ptr)                             :: curl_ptr

        if (present(stat)) stat = -1
        if (len_trim(url) == 0) return

        open (access  = 'stream', &
              action  = 'write', &
              file    = trim(file_path), &
              form    = 'unformatted', &
              iostat  = rc, &
              newunit = file_unit, &
              status  = 'replace')
        if (rc /= 0) return

        curl_ptr = curl_easy_init()
        if (.not. c_associated(curl_ptr)) return

        rc = curl_easy_setopt(curl_ptr, CURLOPT_DEFAULT_PROTOCOL, 'https' // c_null_char)
        rc = curl_easy_setopt(curl_ptr, CURLOPT_URL,              trim(url) // c_null_char)
        rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION,   int( 1, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT,          int(10, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL,         int( 1, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT,   int(10, kind=8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION,    c_funloc(xkcd_png_callback))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA,        c_loc(file_unit))
        rc = curl_easy_perform(curl_ptr)

        call curl_easy_cleanup(curl_ptr)
        close (file_unit)

        if (rc /= CURLE_OK) return
        if (present(stat)) stat = 0
    end subroutine xkcd_fetch_png
end module xkcd

module xwin
    use, intrinsic :: iso_c_binding
    use :: cairo
    use :: xlib
    implicit none
    private

    type, public :: ctx_type
        type(c_ptr)     :: cairo
        type(c_ptr)     :: surface
        type(c_ptr)     :: display
        integer         :: screen
        integer(kind=8) :: root
        integer(kind=8) :: window
        integer(kind=8) :: wm_delete_window
    end type ctx_type

    type, public :: image_type
        type(c_ptr) :: surface
        integer     :: width
        integer     :: height
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
        integer(kind=8)                         :: black, white
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

program main
    use :: cairo
    use :: xkcd
    use :: xlib
    use :: xwin
    implicit none
    character(len=*), parameter :: PNG_FILE   = '/tmp/xkcd.png'

    character(len=256)   :: title
    integer              :: num
    integer              :: rc
    integer(kind=8)      :: client_data(5)
    type(ctx_type)       :: ctx
    type(image_type)     :: image
    type(x_event)        :: event
    type(xkcd_data_type) :: xkcd_data

    num = get_opt()

    call xkcd_fetch_json(num, xkcd_data, rc)
    if (rc /= 0) stop 'Error: Fetching JSON file failed.'
    if (xkcd_data%img(len(xkcd_data%img) - 3:) /= '.png') stop 'Error: Image not in PNG format.'

    call xkcd_fetch_png(xkcd_data%img, PNG_FILE, rc)
    if (rc /= 0) stop 'Error: Fetching PNG file failed.'

    if (rc == 0) then
        print '(a, " (", i0, ")")', xkcd_data%title, xkcd_data%num
        print '("Alt: ", a)', xkcd_data%alt

        write (title, '("xkcd (", i0, ") - ", a)', iostat=rc) xkcd_data%num, xkcd_data%title

        if (rc == 0) call xwin_load_png(image, PNG_FILE, rc)
        if (rc == 0) call xwin_create(ctx, image%width, image%height, trim(title), rc)
        if (rc == 0) call xwin_set_png(ctx, image, 0, 0)
    end if

    do while (rc == 0)
        call x_next_event(ctx%display, event)

        select case (event%type)
            case (EXPOSE)
                call cairo_paint(ctx%cairo)

            case (CLIENT_MESSAGE)
                client_data = transfer(event%x_client_message%data, client_data)
                if (client_data(1) == ctx%wm_delete_window) rc = 1

            case (KEY_PRESS)
                if (event%x_key%keycode == 9) rc = 1
        end select
    end do

    call cairo_surface_destroy(image%surface)
    call xwin_destroy(ctx)
contains
    integer function get_opt() result(num)
        character(len=8) :: arg
        integer          :: rc

        num = 0
        if (command_argument_count() /= 1) return
        call get_command_argument(1, arg)
        read (arg, '(i8)', iostat=rc) num
    end function get_opt
end program main
