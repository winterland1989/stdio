{-# LANGUAGE DeriveDataTypeable #-}

module System.IO.Socket.Types where

import Data.Typeable
import Foreign.C
import Foreign

#include "HsNet.h"

newtype SocketFamily = SocketFamily CInt deriving (Show, Read, Eq, Ord, Typeable)
newtype SocketType = SocketType CInt deriving (Show, Read, Eq, Ord, Typeable)
newtype SocketProtocol = SocketProtocol CInt deriving (Show, Read, Eq, Ord, Typeable)

instance Storable SocketFamily where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketFamily `fmap` peek (castPtr ptr)             
    poke ptr (SocketFamily v) = poke (castPtr ptr) v

instance Storable SocketType where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketType `fmap` peek (castPtr ptr)             
    poke ptr (SocketType v) = poke (castPtr ptr) v

instance Storable SocketProtocol where                      
    sizeOf _ = sizeOf (undefined :: CInt)       
    alignment _ = alignment (undefined :: CInt) 
    peek ptr = SocketProtocol `fmap` peek (castPtr ptr)             
    poke ptr (SocketProtocol v) = poke (castPtr ptr) v

--------------------------------------------------------------------------------

-- | unspecified
aF_UNSPEC           = SocketFamily (#const AF_UNSPEC          )
#ifdef AF_UNIX
-- | local to host (pipes, portals
aF_UNIX             = SocketFamily (#const AF_UNIX            )
#endif
#ifdef AF_INET
-- | internetwork: UDP, TCP, etc
aF_INET             = SocketFamily (#const AF_INET            )
#endif
#ifdef AF_INET6
-- | Internet Protocol version 6
aF_INET6            = SocketFamily (#const AF_INET6           )
#endif
#ifdef AF_IMPLINK
-- | arpanet imp addresses
aF_IMPLINK          = SocketFamily (#const AF_IMPLINK         )
#endif
#ifdef AF_PUP
-- | pup protocols: e.g. BSP
aF_PUP              = SocketFamily (#const AF_PUP             )
#endif
#ifdef AF_CHAOS
-- | mit CHAOS protocols
aF_CHAOS            = SocketFamily (#const AF_CHAOS           )
#endif
#ifdef AF_NS
-- | XEROX NS protocols
aF_NS               = SocketFamily (#const AF_NS              )
#endif
#ifdef AF_NBS
-- | nbs protocols
aF_NBS              = SocketFamily (#const AF_NBS             )
#endif
#ifdef AF_ECMA
-- | european computer manufacturers
aF_ECMA             = SocketFamily (#const AF_ECMA            )
#endif
#ifdef AF_DATAKIT
-- | datakit protocols
aF_DATAKIT          = SocketFamily (#const AF_DATAKIT         )
#endif
#ifdef AF_CCITT
-- | CCITT protocols, X.25 etc
aF_CCITT            = SocketFamily (#const AF_CCITT           )
#endif
#ifdef AF_SNA
-- | IBM SNA
aF_SNA              = SocketFamily (#const AF_SNA             )
#endif
#ifdef AF_DECnet
-- | DECnet
aF_DECnet           = SocketFamily (#const AF_DECnet          )
#endif
#ifdef AF_DLI
-- | Direct data link interface
aF_DLI              = SocketFamily (#const AF_DLI             )
#endif
#ifdef AF_LAT
-- | LAT
aF_LAT              = SocketFamily (#const AF_LAT             )
#endif
#ifdef AF_HYLINK
-- | NSC Hyperchannel
aF_HYLINK           = SocketFamily (#const AF_HYLINK          )
#endif
#ifdef AF_APPLETALK
-- | Apple Talk
aF_APPLETALK        = SocketFamily (#const AF_APPLETALK       )
#endif
#ifdef AF_ROUTE
-- | Internal Routing Protocol
aF_ROUTE            = SocketFamily (#const AF_ROUTE           )
#endif
#ifdef AF_NETBIOS
-- | NetBios-style addresses
aF_NETBIOS          = SocketFamily (#const AF_NETBIOS         )
#endif
#ifdef AF_NIT
-- | Network Interface Tap
aF_NIT              = SocketFamily (#const AF_NIT             )
#endif
#ifdef AF_802
-- | IEEE 802.2, also ISO 8802
aF_802              = SocketFamily (#const AF_802             )
#endif
#ifdef AF_ISO
-- | ISO protocols
aF_ISO              = SocketFamily (#const AF_ISO             )
#endif
#ifdef AF_OSI
-- | umbrella of all families used by OSI
aF_OSI              = SocketFamily (#const AF_OSI             )
#endif
#ifdef AF_NETMAN
-- | DNA Network Management
aF_NETMAN           = SocketFamily (#const AF_NETMAN          )
#endif
#ifdef AF_X25
-- | CCITT X.25
aF_X25              = SocketFamily (#const AF_X25             )
#endif
#ifdef AF_AX25
aF_AX25             = SocketFamily (#const AF_AX25            )
#endif
#ifdef AF_OSINET
-- | AFI
aF_OSINET           = SocketFamily (#const AF_OSINET          )
#endif
#ifdef AF_GOSSIP
-- | US Government OSI
aF_GOSSIP           = SocketFamily (#const AF_GOSSIP          )
#endif
#ifdef AF_IPX
-- | Novell Internet Protocol
aF_IPX              = SocketFamily (#const AF_IPX             )
#endif
#ifdef Pseudo_AF_XTP
-- | eXpress Transfer Protocol (no AF)
pattern Pseudo_AF_XTP       = SocketFamily (#const Pseudo_AF_XTP      )
#endif
#ifdef AF_CTF
-- | Common Trace Facility
aF_CTF              = SocketFamily (#const AF_CTF             )
#endif
#ifdef AF_WAN
-- | Wide Area Network protocols
aF_WAN              = SocketFamily (#const AF_WAN             )
#endif
#ifdef AF_SDL
-- | SGI Data Link for DLPI
aF_SDL              = SocketFamily (#const AF_SDL             )
#endif
#ifdef AF_NETWARE
aF_NETWARE          = SocketFamily (#const AF_NETWARE         )
#endif
#ifdef AF_NDD
aF_NDD              = SocketFamily (#const AF_NDD             )
#endif
#ifdef AF_INTF
-- | Debugging use only
aF_INTF             = SocketFamily (#const AF_INTF            )
#endif
#ifdef AF_COIP
-- | connection-oriented IP, aka ST II
aF_COIP             = SocketFamily (#const AF_COIP            )
#endif
#ifdef AF_CNT
-- | Computer Network Technology
aF_CNT              = SocketFamily (#const AF_CNT             )
#endif
#ifdef Pseudo_AF_RTIP
-- | Help Identify RTIP packets
pattern Pseudo_AF_RTIP      = SocketFamily (#const Pseudo_AF_RTIP     )
#endif
#ifdef Pseudo_AF_PIP
-- | Help Identify PIP packets
pattern Pseudo_AF_PIP       = SocketFamily (#const Pseudo_AF_PIP      )
#endif
#ifdef AF_SIP
-- | Simple Internet Protocol
aF_SIP              = SocketFamily (#const AF_SIP             )
#endif
#ifdef AF_ISDN
-- | Integrated Services Digital Network
aF_ISDN             = SocketFamily (#const AF_ISDN            )
#endif
#ifdef Pseudo_AF_KEY
-- | Internal key-management function
pattern Pseudo_AF_KEY       = SocketFamily (#const Pseudo_AF_KEY      )
#endif
#ifdef AF_NATM
-- | native ATM access
aF_NATM             = SocketFamily (#const AF_NATM            )
#endif
#ifdef AF_ARP
-- | (rev.) addr. res. prot. (RFC 826)
aF_ARP              = SocketFamily (#const AF_ARP             )
#endif
#ifdef Pseudo_AF_HDRCMPLT
-- | Used by BPF to not rewrite hdrs in iface output
pattern Pseudo_AF_HDRCMPLT  = SocketFamily (#const Pseudo_AF_HDRCMPLT )
#endif
#ifdef AF_ENCAP
aF_ENCAP            = SocketFamily (#const AF_ENCAP           )
#endif
#ifdef AF_LINK
-- | Link layer interface
aF_LINK             = SocketFamily (#const AF_LINK            )
#endif
#ifdef AF_RAW
-- | Link layer interface
aF_RAW              = SocketFamily (#const AF_RAW             )
#endif
#ifdef AF_RIF
-- | raw interface
aF_RIF              = SocketFamily (#const AF_RIF             )
#endif
#ifdef AF_NETROM
-- | Amateur radio NetROM
aF_NETROM           = SocketFamily (#const AF_NETROM          )
#endif
#ifdef AF_BRIDGE
-- | multiprotocol bridge
aF_BRIDGE           = SocketFamily (#const AF_BRIDGE          )
#endif
#ifdef AF_ATMPVC
-- | ATM PVCs
aF_ATMPVC           = SocketFamily (#const AF_ATMPVC          )
#endif
#ifdef AF_ROSE
-- | Amateur Radio X.25 PLP
aF_ROSE             = SocketFamily (#const AF_ROSE            )
#endif
#ifdef AF_NETBEUI
-- | 802.2LLC
aF_NETBEUI          = SocketFamily (#const AF_NETBEUI         )
#endif
#ifdef AF_SECURITY
-- | Security callback pseudo AF
aF_SECURITY         = SocketFamily (#const AF_SECURITY        )
#endif
#ifdef AF_PACKET
-- | Packet family
aF_PACKET           = SocketFamily (#const AF_PACKET          )
#endif
#ifdef AF_ASH
-- | Ash
aF_ASH              = SocketFamily (#const AF_ASH             )
#endif
#ifdef AF_ECONET
-- | Acorn Econet
aF_ECONET           = SocketFamily (#const AF_ECONET          )
#endif
#ifdef AF_ATMSVC
-- | ATM SVCs
aF_ATMSVC           = SocketFamily (#const AF_ATMSVC          )
#endif
#ifdef AF_IRDA
-- | IRDA sockets
aF_IRDA             = SocketFamily (#const AF_IRDA            )
#endif
#ifdef AF_PPPOX
-- | PPPoX sockets
aF_PPPOX            = SocketFamily (#const AF_PPPOX           )
#endif
#ifdef AF_WANPIPE
-- | Wanpipe API sockets
aF_WANPIPE          = SocketFamily (#const AF_WANPIPE         )
#endif
#ifdef AF_BLUETOOTH
-- | bluetooth sockets
aF_BLUETOOTH        = SocketFamily (#const AF_BLUETOOTH       )
#endif
#ifdef AF_CAN
-- | Controller Area Network
aF_CAN              = SocketFamily (#const AF_CAN             )
#endif

--------------------------------------------------------------------------------

#ifdef SOCK_STREAM
sOCK_STREAM = SocketType (#const SOCK_STREAM)
#endif
#ifdef SOCK_DGRAM
sOCK_DGRAM = SocketType (#const SOCK_DGRAM) 
#endif
#ifdef SOCK_RAW
sOCK_RAW = SocketType (#const SOCK_RAW) 
#endif
#ifdef SOCK_RDM
sOCK_RDM = SocketType (#const SOCK_RDM) 
#endif
#ifdef SOCK_SEQPACKET
sOCK_SEQPACKET = SocketType (#const SOCK_SEQPACKET) 
#endif

--------------------------------------------------------------------------------

protocol_TCP :: SocketProtocol
protocol_TCP = SocketProtocol (#const IPPROTO_TCP)

protocol_UDP :: SocketProtocol
protocol_UDP = SocketProtocol (#const IPPROTO_UDP)
