module cairo
    use, intrinsic :: iso_c_binding
    implicit none (type, external)
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
        ! cairo_t *cairo_create(cairo_surface_t *target)
        function cairo_create(target) bind(c, name='cairo_create')
            import :: c_ptr
            type(c_ptr), intent(in), value :: target
            type(c_ptr)                    :: cairo_create
        end function cairo_create

        ! cairo_surface_t *cairo_image_surface_create_from_png(const char *filename)
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

        ! cairo_surface_t *cairo_xlib_surface_create(Display *dpy, Drawable drawable, Visual *visual, int width, int height)
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
