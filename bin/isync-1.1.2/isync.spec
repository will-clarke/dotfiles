Summary: Utility to synchronize IMAP mailboxes with local maildir folders
Name: isync
Version: 1.1.2
Release: 1
License: GPL
Group: Applications/Internet
Source: isync-1.1.2.tar.gz
URL: http://isync.sf.net/
Packager: Oswald Buddenhagen <ossi@users.sf.net>
BuildRoot: /var/tmp/%{name}-buildroot

%description
isync is a command line utility which synchronizes mailboxes; currently
Maildir and IMAP4 mailboxes are supported.
New messages, message deletions and flag changes can be propagated both ways.
It is useful for working in disconnected mode, such as on a laptop or with a
non-permanent internet collection (dIMAP).

%prep
%setup
%build
%configure

%install
rm -rf $RPM_BUILD_ROOT
make DESTDIR=$RPM_BUILD_ROOT install
rm -rf $RPM_BUILD_ROOT%{_docdir}/%{name}

%clean
rm -rf $RPM_BUILD_ROOT

%files
%doc AUTHORS COPYING NEWS README TODO ChangeLog src/mbsyncrc.sample src/compat/isyncrc.sample
%{_bindir}/*
%{_mandir}/man1/*
