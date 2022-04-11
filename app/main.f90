program main
    use, intrinsic :: iso_fortran_env, only: i8 => int64
    use :: cairo
    use :: xkcd
    use :: xlib
    use :: xwin
    implicit none (type, external)
    character(len=*), parameter :: PNG_FILE = '/tmp/xkcd.png'

    character(len=256)   :: title
    integer              :: num
    integer              :: rc
    integer(kind=i8)     :: client_data(5)
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

    print '(a, " (", i0, ")")', xkcd_data%title, xkcd_data%num
    print '("Alt: ", a)', xkcd_data%alt

    write (title, '("xkcd (", i0, ") - ", a)', iostat=rc) xkcd_data%num, xkcd_data%title

    if (rc == 0) call xwin_load_png(image, PNG_FILE, rc)
    if (rc == 0) call xwin_create(ctx, image%width, image%height, trim(title), rc)
    if (rc == 0) call xwin_set_png(ctx, image, 0, 0)

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
