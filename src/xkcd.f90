module xkcd
    use, intrinsic :: iso_c_binding
    use :: curl
    implicit none (type, external)
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
        character(len=72)                                   :: url
        integer                                             :: rc
        logical                                             :: found
        type(c_ptr)                                         :: curl_ptr
        type(json_file)                                     :: json

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
        rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION,   int( 1, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT,          int(10, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL,         int( 1, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT,   int(10, kind=i8))
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
        rc = curl_easy_setopt(curl_ptr, CURLOPT_FOLLOWLOCATION,   int( 1, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_TIMEOUT,          int(10, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_NOSIGNAL,         int( 1, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_CONNECTTIMEOUT,   int(10, kind=i8))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEFUNCTION,    c_funloc(xkcd_png_callback))
        rc = curl_easy_setopt(curl_ptr, CURLOPT_WRITEDATA,        c_loc(file_unit))
        rc = curl_easy_perform(curl_ptr)

        call curl_easy_cleanup(curl_ptr)
        close (file_unit)

        if (rc /= CURLE_OK) return
        if (present(stat)) stat = 0
    end subroutine xkcd_fetch_png
end module xkcd
