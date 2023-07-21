module xkcd
    use http, only : request, response_type
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

contains

    subroutine xkcd_fetch_json(num, xkcd_data, stat)
        use :: json_module
        integer,                      intent(in)            :: num
        type(xkcd_data_type),         intent(out)           :: xkcd_data
        integer,                      intent(out), optional :: stat

        type(response_type) :: response
        character(len=72) :: url
        integer           :: rc
        logical           :: found
        type(json_file)   :: json

        if (present(stat)) stat = -1

        if (num > 0) then
            write (url, '(a, i0, "/", a)', iostat=rc) API_BASE, num, API_JSON
        else
            write (url, '(2a)', iostat=rc) API_BASE, API_JSON
        end if

        if (rc /= 0) return

        response = request(url=trim(url))
        xkcd_data%json = response%content

        if (.not. allocated(xkcd_data%json)) return

        call json%initialize()
        call json%deserialize(xkcd_data%json)

        json_block: block
            call json%get('month',      xkcd_data%month,      found); if (.not. found) exit json_block
            call json%get('num',        xkcd_data%num,        found); if (.not. found) exit json_block
            call json%get('link',       xkcd_data%link,       found); if (.not. found) exit json_block
            call json%get('year',       xkcd_data%year,       found); if (.not. found) exit json_block
            call json%get('news',       xkcd_data%news,       found); if (.not. found) exit json_block
            call json%get('safe_title', xkcd_data%safe_title, found); if (.not. found) exit json_block
            call json%get('transcript', xkcd_data%transcript, found); if (.not. found) exit json_block
            call json%get('alt',        xkcd_data%alt,        found); if (.not. found) exit json_block
            call json%get('img',        xkcd_data%img,        found); if (.not. found) exit json_block
            call json%get('title',      xkcd_data%title,      found); if (.not. found) exit json_block
            call json%get('day',        xkcd_data%day,        found); if (.not. found) exit json_block
        end block json_block

        call json%destroy()

        if (found .and. present(stat)) stat = 0
    end subroutine xkcd_fetch_json

    subroutine xkcd_fetch_png(url, file_path, stat)
        character(len=*), intent(in)            :: url
        character(len=*), intent(in)            :: file_path
        integer,          intent(out), optional :: stat

        type(response_type) :: response
        integer, target :: file_unit
        integer         :: rc
        integer :: status

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

        response = request(url=trim(url))

        inquire (unit=file_unit, iostat=status)
        if (status /= 0) return

        write (file_unit, iostat=stat) response%content
        if (stat /= 0) return

        close (file_unit)

        if (.not. response%ok) return
        if (present(stat)) stat = 0
    end subroutine xkcd_fetch_png
end module xkcd
