/*
 * Copyright 2002 the Excalibur contributors (http://excalibar.sourceforge.net/)
 *
 * Portions of this software are based on the work of Slicer/Hackersquest.
 * Those portions, Copyright 2001 Slicer/Hackersquest <slicer@hackersquest.org)
 * 
 * This file is part of Excalibur.
 *
 * Excalibur is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Excalibur is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */


#include "exSniffer.h"
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/in_systm.h>
#include <netinet/ip.h>
#include <netinet/if_ether.h>
#include <netinet/udp.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <sched.h>
#include <netdb.h>

#ifdef __APPLE__
#define __FreeBSD__
#endif

#if !defined(BSD) && !defined(_WIN32)
# define MUST_DO_SELECT
#endif

#ifdef MUST_DO_SELECT
#include <sys/time.h>
#include <unistd.h>
#endif

#ifndef _WIN32
#include <sys/time.h>
#include <sys/resource.h>
#endif

/*
 * Important notes for would be hackers :)
 * This class is dual-threaded
 * Meaning that run() and event() run in separate threads
 * Since missing a single packet causes critical failure (TCP desync),
 * never ever add ANY processing whatsoever to the run() or handlePacket() functions
 * Further work on TCP and UDP dissection (to recreate proper stream) should be
 * added to processPacket() and fucntions called from it.
 *
 * One more thing. This entire file has been written to be "Slow but safe".
 * A modern computer has several GB bandwidth to and from main memory.
 * Copying 30 kb / second back and forth does not hamper performance. 
 *
 * LET THIS CODE STAY CLEAN!
 *
 */

void exCallback (u_char * param, const struct pcap_pkthdr *ph, const u_char *data) {
  exSniffer *u;

  u=(exSniffer *)param;
  u->handlePacket(ph, data);
#ifdef __APPLE__
  qApp->postEvent( (QObject*) param,
    (QEvent*) new QCustomEvent( EXSNIFFER_EVENT_TYPE ) );
#endif
}

exPacketEvent::exPacketEvent(exPacket *p)
  : QCustomEvent(EXSNIFFER_EVENT_PACKET) {
  pck = p;
}

exPacketEvent::~exPacketEvent() {
  if (pck)
    delete pck;
}

exPacket *exPacketEvent::getPacket() const {
  return pck;
}


exTCP::exTCP(unsigned int seq) {
  baseseq=seq;
  nextseq=0;
  lost=0;
  frags.setAutoDelete(true);
}

/*
 * Deliberately written as slow but safe
 */

bool exTCP::add(QByteArray *tcppacket) {
  bool found;
  bool newdata;
  unsigned int dofs;
  unsigned int dlen;
  unsigned int osize;
  unsigned int i;
  exTCPFragment *f;

  f = new exTCPFragment(tcppacket, baseseq);
  if (!f->dlen)
  {
      delete f;
      return false;
  }

  frags.append(f);

  newdata=false;

  do {
    found=false;
    for (f=frags.first(); (f && ! found); f=frags.next()) {
      if ((f->from + f->dlen) < nextseq) {
        qWarning("Seriously out of ordered packet!");
        frags.remove(f);
        found = true;
      } else if (f->from <= nextseq) {
        dofs=nextseq - f->from;
        dlen=f->dlen - dofs;
        if (dlen > 0) {
          newdata = true;
          osize=data.size();
          data.resize(osize+dlen);
          for(i=0;i<dlen;i++)
            data[osize+i]=f->data[dofs+i];
          nextseq += dlen;
        }
        frags.remove(f);
        found = true;
      }
    }
  } while (found);

  if ((frags.count() >= 50) && (frags.count() % 50)==0) {
    lost=frags.count();
    qWarning("Over %d TCP fragments left unassembled! Probably lost some - Stream most likely a lost cause",lost);
  }

  return newdata;
}

exPacket *exTCP::grab(bool serv, exTimeType basetick) {
  exPacket *p;
  unsigned int psize;
  char *rd;
  unsigned char *d;
  int i;
  int nsize;

  if (data.size() < 2) 
    return NULL;

  rd=data.data();
  d=(unsigned char *)rd;

  psize=(d[0]<<8)+d[1];

  if (serv)
    psize+=1;
  else
    psize+=10;
  if (data.size() < psize+2)
    return NULL;

  p=new exPacket(rd+2, psize, serv, false, basetick);

  nsize=data.size()-(psize+2);
  for(i=0;i<nsize;i++) 
    rd[i]=rd[i+psize+2];
  data.resize(nsize);

  return p;
}


exTCPFragment::exTCPFragment(QByteArray *tcppacket, unsigned int baseseq) {
  const char *d;
  const char *dataptr;
  struct tcphdr *tcp;

#ifdef __FreeBSD__
  d = tcppacket->data();
  tcp = (struct tcphdr *) (d+sizeof(struct ip));
  dataptr = (const char *)tcp;
  dataptr += 4 * tcp->th_off;
  from=(ntohl(tcp->th_seq) - baseseq);
  dlen=tcppacket->size() - (dataptr - d);
  data.duplicate(dataptr,dlen);
#else
  /*
  d = tcppacket->data();
  tcp = (struct tcphdr *) (d+sizeof(struct iphdr));
  dataptr = (const char *)tcp;
  dataptr += 4 * tcp->doff;
  from=(ntohl(tcp->seq) - baseseq);
  dlen=tcppacket->size() - (dataptr - d);
  data.duplicate(dataptr,dlen);
  return;
  */

  struct iphdr *ip;
  int ip_hdr_len;
  int ip_tot_len;
  int tcp_hdr_len;
  int total_hdr_len;

  d = tcppacket->data();

    /* d points to an ip header */
  ip = (struct iphdr *)d;
  ip_hdr_len = 4 * ip->ihl;
  ip_tot_len = ntohs(ip->tot_len);

    /* the tcp header begins right after the ip header */
  tcp = (struct tcphdr *)(d + ip_hdr_len);
  tcp_hdr_len = 4 * tcp->doff;

   /* the payload data is at after the tcp header */
  dataptr = (const char *)(tcp) + tcp_hdr_len;

  from=(ntohl(tcp->seq) - baseseq);

  /* the size of the data is the total len stated in the ip header,
     minus the size of the tcp and ip headers */
  total_hdr_len = ip_hdr_len + tcp_hdr_len;
  dlen = ip_tot_len - total_hdr_len;

  if (tcppacket->size() < (dlen + total_hdr_len))
  {
      qWarning("Fragment has short playload.  Dropped.");
      dlen = 0;
  }
  else
      data.duplicate(dataptr,dlen);
#endif
}


exNet::exNet() {
  updateTick();
  client = server = NULL;
  basetick = exTick;
}

exNet::~exNet() {
  if (c)
    delete c;
  if (client)
    delete client;
  if (server)
    delete server;
}

exSniffer::exSniffer(bool doreal, bool dolink, bool docapture) {
  realtime=doreal;
  link=dolink;
  capture=docapture;
  determineLocalIPs();
}

void exSniffer::add(QByteArray *p) {
  m.lock();
  packets.append(p);
  m.unlock();
}

QByteArray *exSniffer::get() {
  QByteArray *p;

  p=NULL;
  m.lock();
  if (! packets.isEmpty())
    p=packets.take(0);
  m.unlock();
  return p;
}

void exSniffer::run() {
  QString f;
  struct bpf_program bpp;
  fd_set fdset;
  struct timeval timeout;
  char buff[4096];
  bool doalert;

#ifndef _WIN32
  struct sched_param sp;
  memset(&sp, 0, sizeof(sp));
  sp.sched_priority=sched_get_priority_min(SCHED_RR) + 1;
  if (realtime) {
    if (pthread_setschedparam(pthread_self(), SCHED_RR, &sp) != 0) {
      qWarning("Failed to make sniffer thread realtime.");
    } else {
      qWarning("Made sniffer thread realtime.");
    }
  } else {
    setpriority(PRIO_PROCESS, getpid(), -10);
  }
#endif

  pcap=pcap_open_live((char*)INTERFACE, 65535, 1, 200, buff);

  if (!pcap) {
    qFatal(QString("pcap failed open: %1").arg(buff));
    return;
  }

  f="ip and (net 208.254.16.0/24 or net 193.252.123.0/24 or net 209.225.26.0/28) and ((tcp and port 10622) or udp)" +
      local_ip_filter;
  qWarning(QString("Filter: %1").arg(f));
  if (pcap_compile(pcap, &bpp, (char *)((const char *)f), 1, 0) == -1) {
     qFatal("Failed to compile pcap filter");
     return;
  }

  if (pcap_setfilter(pcap, &bpp) == -1) {
    qFatal("Failed to apply pcap filter");
    return;
  }

#ifdef __APPLE__
  pcap_loop( pcap, -1, exCallback, (u_char *) this );
#else
  doalert = false;

  while (1) {

#ifdef MUST_DO_SELECT
    FD_ZERO(&fdset);
    FD_SET(pcap_fileno(pcap), &fdset);
    timeout.tv_usec=0;
    if (doalert)
       timeout.tv_sec = 0;
    else
       timeout.tv_sec = 2;
    if (select(pcap_fileno(pcap)+1, &fdset, NULL, NULL, &timeout) != 0) {
#endif
      pcap_dispatch(pcap, 0, exCallback, (u_char *) this);
      doalert = true;
#ifdef MUST_DO_SELECT
    } else if (doalert) {
      doalert = false;
      postEvent(this, new QCustomEvent(EXSNIFFER_EVENT_TYPE));
    }
#endif
  }
#endif
  pcap_close(pcap);
}

void exSniffer::handlePacket(const struct pcap_pkthdr *ph, const u_char *data) {
#ifdef __FreeBSD__
  struct ip *iph;
#else
  struct iphdr *ip;
#endif
  QByteArray *ba;

  if (ph->caplen != ph->len) {
    qWarning("Partial packet capture; %d of %d",ph->caplen,ph->len);
    return;
  }

#ifdef __FreeBSD__
  if (ph->caplen < (sizeof(struct ether_header)+sizeof(struct ip))) {
#else
  if (ph->caplen < (sizeof(struct ether_header)+sizeof(struct iphdr))) {
#endif
    qWarning("Packet smaller than needed header. Dropped.");
    return;
  }

  /* We're capturing in cooked mode (the "any" device"), meaning we
   * would like sll.h.. But that's not installed per default, so 
   * we simply add the size of the header here.
   */

#ifdef __FreeBSD__
  iph=(struct ip *)(data+sizeof(struct ether_header));
#else
  // iph=(struct iphdr *)(data+sizeof(struct ether_header));
  ip=(struct iphdr *)(data+16);
#endif

#ifdef __FreeBSD__
  switch (iph->ip_p) {
    case IPPROTO_TCP:
    case IPPROTO_UDP:
#else
  switch (ip->protocol) {
    case SOL_TCP:
    case SOL_UDP:
#endif
      ba=new QByteArray();
#ifdef __FreeBSD__
      ba->duplicate((char *)iph, ntohs(iph->ip_len));
#else
      ba->duplicate((char *)ip, ntohs(ip->tot_len));
#endif
      add(ba);
      break;
    default:
      qWarning("Got non TCP/UDP packet");
      break;
  }
}

void exSniffer::customEvent(QCustomEvent *e) {
  QByteArray *ba;

  if (e->type() == EXSNIFFER_EVENT_TYPE) {
    do {
      ba=get();
      if (ba) {
        processPacket(ba);
        delete ba;
      }
    } while (ba);
    return;
  }
  QObject::customEvent(e);   
}

void exSniffer::processPacket(QByteArray *ba) {
#ifdef __FreeBSD__
  struct ip *iph;
#else
  struct iphdr *ip;
#endif
  struct udphdr *udp;
  struct tcphdr *tcp;
  char *data;
  exNet *n;
  QString from;
  QString to;
  QPtrDictIterator<exNet> di(nets);
  exPacket *p;
  bool serverpck;
  bool checkpackets;
  int len, plen;

  data=ba->data();

#ifdef __FreeBSD__
  iph=(struct ip *) data;
  if (iph->ip_p == IPPROTO_UDP) {
    udp = (struct udphdr *) (data+sizeof(struct ip));
    n = nets.find((void *)iph->ip_dst.s_addr);
    if (! n)
      return;
    len = ntohs(udp->uh_ulen);
    len -= sizeof(struct udphdr);
    data += sizeof(struct ip)+sizeof(struct udphdr);
#else
  ip=(struct iphdr *) data;
  if (ip->protocol == SOL_UDP) {
    udp = (struct udphdr *) (data+sizeof(struct iphdr));
    n = nets.find((void *)ip->daddr);
    if (! n)
      return;
    len = ntohs(udp->len);
    len -= sizeof(struct udphdr);
    data+=sizeof(struct iphdr)+sizeof(struct udphdr);
#endif

    while (len > 2) {
      plen=(data[0]<<8) + data[1] + 3;
      data+=2;
      len-=2;
      if ((len < plen) || (plen < 0)) {
        qWarning("Short UDP Packet");
        return;
      }
      p = new exPacket(data, plen, true, true, n->basetick);
      qApp->postEvent(n->c, new exPacketEvent(p));
      len-=plen;
      data+=plen;
    }
#ifdef __FreeBSD__
  } else if (iph->ip_p == IPPROTO_TCP) {
    tcp=(struct tcphdr *) (data+sizeof(struct ip));
#else
  } else if (ip->protocol == SOL_TCP) {
    tcp=(struct tcphdr *) (data+sizeof(struct iphdr));
#endif
    
#ifdef __FreeBSD__
    if ((tcp->th_flags & TH_SYN) && (tcp->th_flags & TH_ACK)) {
      n=nets.take((void *)iph->ip_dst.s_addr);
#else
    if ((tcp->syn) && (tcp->ack)) {
      n=nets.take((void *)ip->daddr);
#endif
      if (n) {
        delete n;
      }
      n=new exNet();
#ifdef __FreeBSD__
      n->n_client_addr=iph->ip_dst.s_addr;
      n->n_server_addr=iph->ip_src.s_addr;
      n->server=new exTCP(ntohl(tcp->th_seq)+1);
      n->client=new exTCP(ntohl(tcp->th_ack));
#else
      n->n_client_addr=ip->daddr;
      n->n_server_addr=ip->saddr;
      n->server=new exTCP(ntohl(tcp->seq)+1);
      n->client=new exTCP(ntohl(tcp->ack_seq));
#endif
      n->c=new exConnection(n, link, capture);

#ifdef __FreeBSD__
      nets.insert((void *)iph->ip_dst.s_addr, n);
#else
      nets.insert((void *)ip->daddr, n);
#endif

#ifdef __FreeBSD__
    } else if (tcp->th_flags & TH_FIN) {
      n=nets.take((void *)iph->ip_dst.s_addr);
#else
    } else if (tcp->fin) {
      n=nets.take((void *)ip->daddr);
#endif
      if (! n)
#ifdef __FreeBSD__
        n=nets.take((void *)iph->ip_src.s_addr);
#else
        n=nets.take((void *)ip->saddr);
#endif
      if (n) {
        delete n;
      }
    } else {
      // A normal packet? Whoa..
#ifdef __FreeBSD__
      n=nets.find((void *)iph->ip_dst.s_addr);
#else
      n=nets.find((void *)ip->daddr);
#endif
      if (! n)
#ifdef __FreeBSD__
       n=nets.find((void *)iph->ip_src.s_addr);
#else
       n=nets.find((void *)ip->saddr);
#endif
      if (! n)
        return;
      checkpackets=false;
#ifdef __FreeBSD__
      if (iph->ip_src.s_addr == n->n_client_addr) {
#else
      if (ip->saddr == n->n_client_addr) {
#endif
        serverpck=false;
        checkpackets=n->client->add(ba);
      } else {
        serverpck=true;
        checkpackets=n->server->add(ba);
      }
      if (checkpackets) {
        do {
          if (serverpck) 
            p=n->server->grab(true, n->basetick);
          else
            p=n->client->grab(false, n->basetick);
          if (p) {
            qApp->postEvent(n->c, new exPacketEvent(p));
          }
        } while (p);
      }
    }
  }
}

void exSniffer::determineLocalIPs(void)
{
    char hostname[128];
    struct hostent *h;
    struct in_addr **ia;

    local_ip_filter = "";

    if (!prefs.exclude_local)
        return;

    if (gethostname(hostname, sizeof(hostname) - 1) == -1)  {
        qWarning("Could not get host name for local IP filter!");
        return;
    }

    qWarning(QString("Hostname is %1").arg(hostname));

    h = gethostbyname(hostname);
    if ((!h) || (h->h_addrtype != AF_INET || !h->h_addr_list[0]))  {
        qWarning("Could not look up host name for local IP filter!");
        return;
    }

    ia = (in_addr **)h->h_addr_list;
    while (*ia)  {
          /* if we already have a host in the list, add an or */
        if (local_ip_filter.length())
          local_ip_filter.append(" or ");
        local_ip_filter.append("host ").append(inet_ntoa(**ia));
        ia++;
    }  /* while we have addresses */

    local_ip_filter.prepend(" and not (");
    local_ip_filter.append(")");
}

