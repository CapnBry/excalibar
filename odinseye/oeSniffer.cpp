/*
 * Copyright 2001 Slicer/HackersQuest (slicer@hackersquest.org)
 *
 * This file is part of Odin's Eye.
 *
 * Odin's Eye is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Odin's Eye is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */


#include "oeSniffer.h"
#include <netinet/in.h>
#include <sys/types.h>
#include <sys/socket.h>
#include <netinet/ip.h>
#include <netinet/ether.h>
#include <netinet/udp.h>
#include <netinet/tcp.h>
#include <arpa/inet.h>
#include <pthread.h>
#include <sched.h>

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

void oeCallback (u_char * param, const struct pcap_pkthdr *ph, const u_char *data) {
  oeSniffer *u;

  u=(oeSniffer *)param;
  u->handlePacket(ph, data);
}



oePacketEvent::oePacketEvent(oePacket *p)
  : QCustomEvent(OESNIFFER_EVENT_PACKET) {
  pck = p;
}

oePacketEvent::~oePacketEvent() {
  if (pck)
    delete pck;
}

oePacket *oePacketEvent::getPacket() const {
  return pck;
}


oeTCP::oeTCP(unsigned int seq) {
  baseseq=seq;
  nextseq=0;
  lost=0;
  frags.setAutoDelete(true);
}

/*
 * Deliberately written as slow but safe
 */

bool oeTCP::add(QByteArray *tcppacket) {
  bool found;
  bool newdata;
  unsigned int dofs;
  unsigned int dlen;
  unsigned int osize;
  unsigned int i;
  oeTCPFragment *f;

  frags.append(new oeTCPFragment(tcppacket, baseseq));

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

oePacket *oeTCP::grab(bool serv, oeTimeType basetick) {
  oePacket *p;
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

  p=new oePacket(rd+2, psize, serv, false, basetick);

  nsize=data.size()-(psize+2);
  for(i=0;i<nsize;i++) 
    rd[i]=rd[i+psize+2];
  data.resize(nsize);

  return p;
}


oeTCPFragment::oeTCPFragment(QByteArray *tcppacket, unsigned int baseseq) {
  const char *d;
  const char *dataptr;
  struct tcphdr *tcp;

  d=tcppacket->data();
  tcp=(struct tcphdr *) (d+sizeof(struct iphdr));
  dataptr=(const char *)tcp;
  dataptr+=4 * tcp->doff;
  dlen=tcppacket->size() - (dataptr - d);
  from=(ntohl(tcp->seq) - baseseq);
  data.duplicate(dataptr,dlen);
}


oeNet::oeNet() {
  updateTick();
  client = server = NULL;
  basetick = oeTick;
}

oeNet::~oeNet() {
  if (c)
    delete c;
  if (client)
    delete client;
  if (server)
    delete server;
}

oeSniffer::oeSniffer(bool doreal, bool dolink) {
  realtime=doreal;
  link=dolink;
}

void oeSniffer::add(QByteArray *p) {
  m.lock();
  packets.append(p);
  m.unlock();
}

QByteArray *oeSniffer::get() {
  QByteArray *p;

  p=NULL;
  m.lock();
  if (! packets.isEmpty())
    p=packets.take(0);
  m.unlock();
  return p;
}

void oeSniffer::run() {
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

  pcap=pcap_open_live("any", 2000, 0x0100, 250, buff);
  if (!pcap) {
    qFatal(QString("pcap failed open: %1").arg(buff));
    return;
  }

  f="ip and net 208.254.16.0/24 and ((tcp and port 10622) or udp)";
  qWarning(QString("Applying filter: %1").arg(f));
  if (pcap_compile(pcap, &bpp, (char *)((const char *)f), 1, 0) == -1) {
     qFatal("Failed to compile pcap filter");
     return;
  }

  if (pcap_setfilter(pcap, &bpp) == -1) {
    qFatal("Failed to apply pcap filter");
    return;
  }

  doalert = false;

  qWarning("Starting Listen Loops");
  while (1) {

#ifdef MUST_DO_SELECT
    FD_ZERO(&fdset);
    FD_SET(pcap_fileno(pcap), &fdset);
    timeout.tv_usec=0;
    if (doalert)
       timeout.tv_sec = 0;
    else
       timeout.tv_sec=5;
    if (select(pcap_fileno(pcap)+1, &fdset, NULL, NULL, &timeout) != 0) {
#endif
      pcap_dispatch(pcap, 0, oeCallback, (u_char *) this);
      doalert = true;
#ifdef MUST_DO_SELECT
    } else if (doalert) {
      doalert = false;
      postEvent(this, new QCustomEvent(OESNIFFER_EVENT_TYPE));
    }
#endif
  }
  pcap_close(pcap);
}

void oeSniffer::handlePacket(const struct pcap_pkthdr *ph, const u_char *data) {
  struct iphdr *ip;
  QByteArray *ba;

  if (ph->caplen != ph->len) {
    qWarning("Partial packet capture; %d of %d",ph->caplen,ph->len);
    return;
  }

  if (ph->caplen < (sizeof(struct ether_header)+sizeof(struct iphdr))) {
    qWarning("Undersized packet. Dropped.");
    return;
  }

  /* We're capturing in cooked mode (the "any" device"), meaning we
   * would like sll.h.. But that's not installed per default, so 
   * we simply add the size of the header here.
   */

  ip=(struct iphdr *)(data+16);

  switch (ip->protocol) {
    case SOL_TCP:
    case SOL_UDP:
      ba=new QByteArray();
      ba->duplicate((char *)ip, ntohs(ip->tot_len));
      add(ba);
      break;
    default:
      qWarning("Got non TCP/UDP packet");
      break;
  }
}

void oeSniffer::customEvent(QCustomEvent *e) {
  QByteArray *ba;

  if (e->type() == OESNIFFER_EVENT_TYPE) {
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

void oeSniffer::processPacket(QByteArray *ba) {
  struct iphdr *ip;
  struct udphdr *udp;
  struct tcphdr *tcp;
  char *data;
  oeNet *n;
  QString from;
  QString to;
  QPtrDictIterator<oeNet> di(nets);
  oePacket *p;
  bool serverpck;
  bool checkpackets;
  int len, plen;

  data=ba->data();
  ip=(struct iphdr *) data;

  if (ip->protocol == SOL_UDP) {
    udp=(struct udphdr *) (data+sizeof(struct iphdr));
    n=nets.find((void *)ip->daddr);
    if (! n)
      return;
    len=ntohs(udp->len);
    len-=sizeof(struct udphdr);
    data+=sizeof(struct iphdr)+sizeof(struct udphdr);
    while (len > 2) {
      plen=(data[0]<<8) + data[1] + 3;
      data+=2;
      len-=2;
      if ((len < plen) || (plen < 0)) {
        qWarning("Short UDP Packet");
        return;
      }
      p = new oePacket(data, plen, true, true, n->basetick);
      qApp->postEvent(n->c, new oePacketEvent(p));
      len-=plen;
      data+=plen;
    }
  } else if (ip->protocol == SOL_TCP) {
    tcp=(struct tcphdr *) (data+sizeof(struct iphdr));
    
    if ((tcp->syn) && (tcp->ack)) {
      n=nets.take((void *)ip->daddr);
      if (n) {
        delete n;
      }
      n=new oeNet();
      n->n_client_addr=ip->daddr;
      n->n_server_addr=ip->saddr;
      n->server=new oeTCP(ntohl(tcp->seq)+1);
      n->client=new oeTCP(ntohl(tcp->ack_seq));
      n->c=new oeConnection(n, link);

      nets.insert((void *)ip->daddr, n);

    } else if (tcp->fin) {
      n=nets.take((void *)ip->daddr);
      if (! n) 
        n=nets.take((void *)ip->saddr);
      if (n) {
        delete n;
      }
    } else {
      // A normal packet? Whoa..
      n=nets.find((void *)ip->daddr);
      if (! n)
        n=nets.find((void *)ip->saddr);
      if (! n)
        return;
      checkpackets=false;
      if (ip->saddr == n->n_client_addr) {
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
            qApp->postEvent(n->c, new oePacketEvent(p));
          }
        } while (p);
      }
    }
  }
}

