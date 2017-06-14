/* ----------------------------------------------------------------------------
   (c) The University of Glasgow 2006
   
   Useful Win32 bits
   ------------------------------------------------------------------------- */

#if defined(_WIN32)

#include "HsBase.h"

int get_unique_file_info(int fd, HsWord64 *dev, HsWord64 *ino)
{
    HANDLE h = (HANDLE)_get_osfhandle(fd);
    BY_HANDLE_FILE_INFORMATION info;

    if (GetFileInformationByHandle(h, &info))
    {
        *dev = info.dwVolumeSerialNumber;
        *ino = info.nFileIndexLow
             | ((HsWord64)info.nFileIndexHigh << 32);

        return 0;
    }

    return -1;
}

#endif
