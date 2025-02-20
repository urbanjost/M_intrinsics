use, intrinsic :: iso_fortran_env, only : stderr=>ERROR_UNIT, stdout=>OUTPUT_UNIT, stdin=>INPUT_UNIT
use M_intrinsics, only : help_intrinsics
use M_CLI2,       only : set_args, sget, iget, lget, specified, topics=>unnamed
use M_CLI2,       only : set_mode
use M_match,      only : getpat, match, regex_pattern
use M_match,      only : YES, ERR
use M_strings,    only : lower, indent, atleast, str
use M_attr,       only : attr, attr_update
use M_io,         only : filewrite, fileread, get_env
implicit none
type(regex_pattern)            :: p, start_p, end_p
character(len=*),parameter     :: gen='(*(g0:))'
character(len=:),allocatable   :: help_text(:), version_text(:)
character(len=256),allocatable :: manual(:),section(:),clone_no_color(:)
character(len=:),allocatable   :: doc(:)
character(len=:),allocatable   :: regex, start, end
character(len=:),allocatable   :: query
character(len=:),allocatable   :: filename
character(len=:),allocatable   :: templine
character(len=:),allocatable   :: last
character(len=:),allocatable   :: editor
real                           :: rm
integer                        :: i, j, k, m
integer                        :: ilines
integer                        :: lines
integer                        :: iostat
character(len=256)             :: iomsg
character(len=:),allocatable   :: line
integer                        :: iinf
integer                        :: lun
integer                        :: direction
integer                        :: irestore
integer                        :: search_end
logical                        :: number
logical                        :: topic
logical                        :: cmdmode
logical                        :: prefix, ignorecase, demo, color
character(len=512)             :: paws, remember
character(len=20) ::  &
&  bg=       '<E>                ',  &  ! initial background color
&  fg=       '<w>                ',  &  ! text color
&  prg=      '<c>                ',  &  ! demo program text color
&  head=     '<y></bo>           ',  &  ! header line
&  head_=    '</bo>              ',  &
&  fixed=    '<w>                ',  &  ! color of leading > in demo program output
&  output=   '<y>                ',  &  ! demo program output
&  output_=  '</bo>              '
namelist/fman_colors/bg,fg,prg,head,head_,fixed,output,output_
