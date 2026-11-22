# Technischer Statusbericht Stufe 1: Run it in the Cloud

**Team 02** · Cloud Application Project WS 2026/27 · MCI Master DiBSE

**Autor:innen:** Miralem Djedovic, Fabian Egger, Clemens Huber, Romed Kelmer, Thomas Klammer, Christian Plattner

**Stand:** 17.11.2026

---

## 1. Zielerreichung

**Aufgabe der Stufe:** Eine funktionierende, über Web und Game-Client erreichbare Spieleserver-Anwendung auf einer CloudStack-VM, mit kontrolliertem Zugriff. Ziele: IaaS, Compute, Networking, IAM, Sicherheit (5), Datenschutz (6), Betrieb und Dokumentation (10).

**Kurzfassung:** Der Spieleserver „Das Verrückte Labyrinth“ läuft seit dem 09.11.2026 auf der VM `team02-game-01` und ist mit dem Game-Client über das VPN spielbar. Ein Testspiel mit drei Spielern lief am 11.11. bis zum Spielende. Von außen kommen nur die Ports 22 und 8082–8091 durch die Firewall, und seit dem 15.11. nur aus dem VPN. SSH akzeptiert nur Keys. Nach einem Neustart der VM sind die Spieleserver nach etwa 27 s ohne Eingriff wieder in der Lobby, ein abgestürzter Serverprozess nach etwa 8 s. Offen ist die Verschlüsselung der Verbindung.

### Zielwerte

| Nr. | Zielwert | Status |
|---|---|---|
| Z1 | Ein Spiel mit mehreren Spielern läuft über die Cloud-VM von der Lobby bis zum Spielende. | **erreicht** (11.11., 3 Spieler, Abschnitt 4.2) |
| Z2 | Von außen sind nur TCP 22 und 8082–8091 erreichbar, und nur aus dem VPN. | **erreicht** (15.11., Portscan 1–10000, Abschnitt 4.3; VPN-Einschränkung Abschnitt 4.7) |
| Z3 | SSH-Login mit Passwort wird abgelehnt, nur Key-Login geht. | **erreicht** (15.11., Abschnitt 4.4) |
| Z4 | Nach einem Neustart der VM sind die Spieleserver ohne Eingriff wieder in der Lobby. | **erreicht** (15.11., ca. 27 s, Abschnitt 4.5) |
| Z5 | Stürzt der Serverprozess ab, startet er selbst neu. | **erreicht** (15.11., ca. 8 s, Abschnitt 4.6) |

### Erreichungsgrad je Ziel der Stufe

| Ziel | Stand |
|---|---|
| **IaaS / Compute** | **erreicht.** VM `team02-game-01`, Ubuntu 24.04, 2 vCPU, 4 GB RAM (`s2.2c4g`). |
| **Game-Client** | **erreicht**, über VPN. Der Client holt die Serverliste bei der Lobby und verbindet sich mit `ws://172.16.20.14:<Port>`. |
| **Web** | **erreicht.** HTTPS-Lobby für die Serverliste, WebSocket für das Spiel. Ein eigenes Web-Frontend ist laut Rückfrage beim Betreuer nicht gemeint. |
| **Networking** | **erreicht.** Isoliertes Netz `team02-net`, Source NAT über `172.16.20.14`, Firewall und Port-Forwarding für 22 und 8082–8091. |
| **Kontrollierter Zugriff** | **erreicht.** Firewall nur für das NetBird-VPN (`10.98.0.0/16`). Ins VPN kommt nur, wer sich per SSO mit dem mci4me-Konto anmeldet und einem Team-Projekt angehört. VMs anderer Teams und das Internet sind ausgeschlossen (Abschnitt 4.7). |
| **IAM** | **erreicht.** Je Person ein eigener Benutzer mit eigenem Key auf der VM, Passwort-Login aus, Zugang einzeln entziehbar (BA). |
| **Sicherheit (5)** | **teilweise.** Dienst als System-User ohne Login-Shell, Updates eingespielt (BA), nur Key-Login, Firewall auf das VPN beschränkt, Git-Historie auf Zugangsdaten geprüft (Abschnitt 4.8). Offen: unverschlüsselte Verbindung (`ws://`). |
| **Datenschutz (6)** | **erreicht im Rahmen des Projekts.** Nur für den Spielbetrieb nötige Daten, siehe Abschnitt 2. |
| **Betrieb und Doku (10)** | **erreicht.** Automatischer Start nach Neustart (Z4) und nach Absturz (Z5), Logs im Journal, Architekturdiagramm, Betriebsanleitung im Repo, die Anlegen, Betrieb und Aktualisierung Schritt für Schritt beschreibt. |

**Vorgriff auf Stufe 3:** Auf der VM laufen bis zu 10 Spielinstanzen, ein Autoscaler startet und stoppt sie nach Bedarf. Das ist **kein** Nachweis für Elastizität im Sinne von Stufe 3, weil es nur innerhalb einer VM skaliert.

---

## 2. Umsetzung und Entscheidungen

Die Ressourcen wurden im Projekt `team02` über die Web-Oberfläche und die API (`cmk`) von Hand angelegt, die Befehle stehen in der Betriebsanleitung. Infrastruktur als Code folgt in Stufe 2.

| Entscheidung | Begründung |
|---|---|
| **Isoliertes Netz mit Source NAT, Port-Forwarding und Firewall** | Die VM liegt im privaten Netz. Eine Firewall-Regel allein reicht nicht, erst das Port-Forwarding leitet den Port an die VM. |
| **Firewall-Quelle nur VPN (`10.98.0.0/16`) statt `0.0.0.0/0`** | Mit `0.0.0.0/0` konnte jedes andere Team aus `172.16.20.0/24` die VM erreichen. So hängt der Netzzugang an einer persönlichen SSO-Anmeldung, wie es die Plattform-Doku empfiehlt. |
| **Ubuntu 24.04 LTS, Java 25 (`openjdk-25-jre-headless`)** | Der Server braucht Java 25, eine Variante ohne Oberfläche reicht. |
| **JAR lokal bauen und per `scp` hochladen** | Kein Build auf der VM, das spart RAM. |
| **systemd-Dienst, eigener System-User ohne Login-Shell** | Autostart, Neustart nach Absturz, Logs im Journal. Bei einer Lücke ist nur dieser User betroffen. |
| **Keypair beim Deploy mitgeben** | cloud-init übernimmt Keys nur beim ersten Boot, ein nachträglich zugewiesener Key führte zu `Permission denied`. |
| **Ein Benutzer und ein Key je Person** | Nachvollziehbar, wer was getan hat. Zugang einzeln entziehbar. |
| **`server.publichost` = `172.16.20.14`** | Diese Adresse meldet der Server der Lobby. Die interne `10.1.0.158` wäre für Clients nicht erreichbar. |
| **Mehrere Prozesse statt mehrerer Spiele pro Prozess** | Der Server verwaltet pro Prozess genau ein Spiel. Mehrere Prozesse brauchen keine Code-Änderung und bleiben mit den Clients der anderen Gruppen kompatibel. |
| **VM auf 2 vCPU/4 GB vergrößert, bis zu 10 Instanzen mit je 256 MB Heap** | Eine Instanz braucht im Leerlauf ca. 180–230 MB, zehn zusammen ca. 1,9 GB. 100 Instanzen wurden verworfen (ca. 20 GB RAM, Projektlimit 12 GB). |
| **Autoscaler als systemd-Timer (alle 30 s)** | Hält 2 freie Instanzen bereit, stoppt überzählige nach 5 min Leerlauf, liest den Zustand aus der Lobby. |

**Rahmen der Plattform** (`docs.cloud.dibse.at`, Stand 15.11.): Es gibt kein Kubernetes als Dienst und keine VM-Backups, nur Snapshots. Die Plattform läuft auf **einem Server**, Hochverfügbarkeit ist deshalb nur auf VM- und Prozess-Ebene möglich. Limits je Team: 7 VMs, 12 vCPU, 12 GB RAM, 200 GB Speicher, 3 Public-IPs. Belegt sind 1 VM, 2 vCPU, 4 GB RAM, 20 GiB und 1 Public-IP.

**Datenschutz:** Spielstand und Spielernamen liegen nur im Arbeitsspeicher und sind nach einem Neustart weg. Die externe Lobby speichert pro Server nur Name, WebSocket-Adresse, Spielerzahl, Status, ID und Zeitstempel. Zugriff auf die VM haben nur die Teammitglieder per SSH-Key über das VPN. Im Repo wurden keine personenbezogenen E-Mail-Adressen gefunden (Abschnitt 4.8).

---

## 3. Architektur

![Architektur Stufe 1: Game-Client und Administration über das VPN zur Public IP, dahinter Netz und VM mit Spieleservern und Autoscaler, Lobby extern](Architektur_Stufe1.png)

*Abbildung 1: Architektur Stufe 1 (Stand 15.11.2026).*

![Firewall-Regeln der Public IP 172.16.20.14: TCP 22 und 8082–8091 nur aus 10.98.0.0/16](Abbildung%202.1.png)

*Abbildung 2.1: Firewall der Public IP `172.16.20.14`, beide Regeln nur für das VPN (`10.98.0.0/16`).*

![Port-Forwarding der Public IP 172.16.20.14 auf team02-game-01](Abbildung%202.2.png)

*Abbildung 2.2: Port-Forwarding 22 und 8082–8091 auf `team02-game-01` (`10.1.0.158`).*

| Ressource | Wert |
|---|---|
| Projekt / Zone | `team02` / `zone1` (Advanced-Netzwerk) |
| Netz | `team02-net`, Isolated, `10.1.0.0/24`, Offering `isolated-internet` |
| Public IP | `172.16.20.14` (Source NAT) |
| VM | `team02-game-01`, KVM, `s2.2c4g`, Ubuntu 24.04 LTS, 20-GiB-Systemplatte |
| SSH-Keypair | `team02-key` |
| Firewall | TCP 22 und TCP 8082–8091, nur von `10.98.0.0/16` (seit 15.11., vorher `0.0.0.0/0`) |
| Port-Forwarding | 22 → VM:22, 8082–8091 → VM:8082–8091 |

**Technologien und Abhängigkeiten:**

- **Anwendung:** Java 25, Spring Boot 3.5.7, WebSocket-Endpunkt `/game`, Maven-Module `Contracts`, `ManagementClient`, `Client`, `Server`.
- **Lobby (extern, nicht unser Dienst):** Die Instanzen registrieren sich dort, Clients und Autoscaler lesen die Serverliste.
- **VPN (NetBird, MCI):** Ohne VPN ist die Public IP nicht erreichbar.
- **GitHub:** Repo `labyrinth`, CI baut und testet mit GitHub Actions, deployt aber nicht.

---

## 4. Verifikation

Gemessen am 15.11.2026 von einem Laptop im NetBird-VPN gegen die eigene Public IP, sofern nicht anders angegeben. Uhrzeiten in MEZ.

### 4.1 Übersicht

| Datum | Test | Ergebnis | Abschnitt |
|---|---|---|---|
| 09.11. | Dienst gestartet, Client verbindet sich | Lobby zeigt `Team02 Cloud`, Verbindung ok | BA |
| 11.11. | Spiel mit 3 echten Clients (Z1) | bis `GAME_OVER`, alle sauber getrennt | 4.2 |
| 11.11. | Autoscaler (Leerlauf, VM-Neustart, Spieler verbindet sich) | stoppt überzählige, startet bei Bedarf eine weitere | BA |
| 15.11. | Portscan 1–10000 (Z2) | nur 22 und 8082–8091 offen | 4.3 |
| 15.11. | SSH mit Passwort (Z3) | nur `publickey` angeboten | 4.4 |
| 15.11. | Neustart der VM (Z4) | Lobby nach 27 s wieder da | 4.5 |
| 15.11. | Absturz eines Serverprozesses (Z5) | nach 7,8 s wieder spielbereit | 4.6 |
| 15.11. | WebSocket-Handshake | `101 Switching Protocols` | 4.6 |
| 15.11./17.11. | Firewall auf VPN, Test mit und ohne VPN | mit VPN offen, ohne VPN Timeout | 4.7 |
| 15.11. | Git-Historie auf Zugangsdaten | keine Schlüssel, ein Standard-Token gefunden | 4.8 |

### 4.2 Z1: Spiel mit mehreren Spielern

Drei Instanzen des echten Game-Clients (Test1 bis Test3) verbanden sich am 11.11. um 19:45 über das VPN mit „Team02 Cloud“ und spielten bis `GAME_OVER` (Gewinner Test3, alle 6 Schätze). Danach waren alle zurück in der Lobby und sauber getrennt. Davor liefen ein Protokolltest mit 2 Spielern und ein Test mit 1 Spieler (BA, Testprotokoll). **Z1 ist erfüllt.** Grenze: ein einzelnes Testspiel.

![Serverliste im Game-Client mit Team02 Cloud / Autoscale #1 und #2](Abbildung%203.png)

*Abbildung 3: Serverliste im Game-Client über das VPN (21.11.2026): die beiden freien Instanzen „Team02 Cloud / Autoscale #1“ und „#2“ stehen in der Lobby.*

### 4.3 Z2: Portscan der eigenen Public IP

TCP-Verbindungsversuch auf die Ports 1–10000, zuerst mit `nc -vz` (1 s Timeout), dann zur Kontrolle mit `nmap -Pn -sT -p 1-10000 --max-retries 1 --reason`. Gescannt wurde nur die eigene IP.

| Zustand | Ports | Anzahl |
|---|---|---|
| **offen** | 22, 8082, 8084 | 3 |
| **refused** (erreicht die VM, dort lauscht nichts) | 8083, 8085–8091 | 8 |
| **gefiltert** (von der Firewall verworfen) | alle übrigen | 9.989 |

Beide Werkzeuge liefern dasselbe Ergebnis, nur 22 und 8082–8091 passieren die Firewall. Von den Spielports lauschten zum Messzeitpunkt nur die zwei laufenden Instanzen. **Z2 ist erfüllt.** Grenzen: nur TCP, nur Ports 1–10000, eine Messung.

### 4.4 Z3: SSH ohne Key

SSH-Verbindung als `ubuntu` mit erzwungener Passwort-Methode (`-o PubkeyAuthentication=no -o PreferredAuthentications=password`):

```
debug1: Authentications that can continue: publickey
ubuntu@172.16.20.14: Permission denied (publickey).
```

Der Server bietet nur `publickey` an. **Z3 ist erfüllt** (von außen nachgewiesen). Der Host-Key stimmt mit dem in der Betriebsanleitung dokumentierten überein.

### 4.5 Z4: Neustart der VM

Ein Skript prüfte jede Sekunde die Ports 22 und 8082–8084 und alle 3 s die Lobby. Vorher standen beide Instanzen auf `LOBBY`. Neustart über „Reboot Instance“ in CloudStack (normal, ohne Force).

| Uhrzeit | Ereignis | seit Klick |
|---|---|---|
| 10:20:01 | Neustart ausgelöst (CloudStack-Event `VM.REBOOT`) | 0 s |
| 10:20:03 | Ports zu, Lobby-Einträge verschwinden (Abmeldung beim Herunterfahren) | +2 s |
| 10:20:22 | SSH wieder offen | +21 s |
| 10:20:29 | Ports 8082 und 8083 offen | +28 s |
| 10:20:31 | Beide Instanzen wieder in der Lobby | +30 s |

Vom ersten Ausfall bis zur Lobby vergingen **27 s**, ohne Eingriff. **Z4 ist erfüllt.** Die Instanzen melden sich beim Herunterfahren selbst ab, es bleiben keine veralteten Einträge. Grenze: ein normaler Neustart ohne laufendes Spiel, kein Stromausfall.

### 4.6 Z5: Absturz eines Serverprozesses und Erreichbarkeit

Auf der VM wurde der Prozess von Instanz #1 (Port 8082) mit `kill -9` hart beendet. Der Dienst ist mit `Restart=on-failure` und 5 s Wartezeit konfiguriert.

```
13:53:07 systemd[1]: labyrinth@1.service: Main process exited, code=killed, status=9/KILL
13:53:12 systemd[1]: labyrinth@1.service: Scheduled restart job, restart counter is at 1.
13:53:15 ... ServerRegistrationService : Server successfully registered.
```

Nach **7,8 s** lauschte Port 8082 wieder und die Instanz stand in der Lobby (Journal in UTC, vollständige Ausgabe in `cloud-project/absturztest_2026-11-15.txt`). **Z5 ist erfüllt.** Nebenbei behoben: Vom Autoscaler gestoppte Instanzen galten in systemd als `failed`, weil Java bei `SIGTERM` mit 143 endet. Die Dienstvorlage wertet das jetzt als normales Ende (`SuccessExitStatus=143`). Grenzen: Ein laufendes Spiel geht beim Absturz verloren (Spielstand nur im Speicher). Ein hängender Prozess ohne Absturz wird nicht erkannt.

**Erreichbarkeit:** Ein WebSocket-Upgrade auf `ws://172.16.20.14:8082/game` liefert `101 Switching Protocols` und die `SERVER_INFO` des Servers.

### 4.7 Firewall auf das VPN eingeschränkt

Vorher erlaubten zwei Regeln TCP 22 und 8082–8091 von `0.0.0.0/0`, also auch jedem anderen Team aus `172.16.20.0/24`. Am 15.11. wurden mit `cmk` zuerst neue Regeln mit Quelle `10.98.0.0/16` angelegt, der Zugang getestet und erst dann die alten gelöscht, sodass niemand ausgesperrt wurde.

| Test | Erwartet | Ergebnis |
|---|---|---|
| `cmk listFirewallRules` | nur Regeln mit `10.98.0.0/16` | 2 Regeln, `Active` |
| SSH und Port 8082 über VPN | erreichbar | ok |
| TCP 22 und 8082 ohne VPN (17.11., `netbird down`, 5 s Timeout) | Timeout | Timeout, nach `netbird up` wieder offen |
| Verbindung von der VM eines anderen Teams | Timeout | nicht getestet |

Der Test ohne VPN zeigt, dass die VM vom Internet aus nicht erreichbar ist. Dass auch andere Teams abgewiesen werden, belegen die Firewall-Regeln, ein Test von einer fremden VM fehlt. **Bewertung:** Zwei Schutzschichten, das Netz (VPN plus Firewall) und der Login (SSH-Key je Person). Grenze: Alle VPN-Nutzer der Plattform kommen aus `10.98.0.0/16`, auch Studierende anderer Teams. Für SSH reicht das, weil nur unsere Keys angenommen werden. Die Spielports sind für sie offen, was für einen Spieleserver gewollt ist.

### 4.8 Git-Historie auf Zugangsdaten durchsucht

Die gesamte Historie des öffentlichen Repos (417 Commits, alle Branches) wurde per Mustersuche auf private Schlüssel, Tokens, Passwörter und Zugangsdaten in URLs geprüft. Gefunden wurde nur ein Standardwert für ein Deploy-Token in `Labyrinth.Server/deploy.js`, das unser Aufbau nicht nutzt. Er ist seit dem 17.11. aus dem Code entfernt, das Skript verlangt das Token jetzt als Argument oder Umgebungsvariable. Grenze: Die Suche ist kein spezialisiertes Werkzeug, Binärdateien wurden nicht geprüft.

---

## 5. Offene Punkte und Reflexion

| Punkt | Einordnung |
|---|---|
| **Unverschlüsselte Verbindung** | Clients verbinden sich mit `ws://`. TLS braucht einen vorgeschalteten Proxy und ein Zertifikat, geplant über `team02.app.dibse.at`. |
| **Negativtest von einer fremden VM** | Die Firewall-Regeln schließen andere Teams aus, der Test von einer VM eines anderen Teams steht noch aus. |
| **Spieler anderer Teams** | Andere Teams erreichen unsere Spieleserver nur über das VPN. Falls sie von ihren VMs aus zugreifen sollen, wird für 8082–8091 zusätzlich `172.16.20.0/24` erlaubt, SSH bleibt nur über VPN. |
| **Deploy-Token in der Git-Historie** | Aus dem Code entfernt (Abschnitt 4.8), steht aber noch in alten Commits. Falls der Endpunkt noch aktiv ist, muss das Token dort geändert werden. |
| **Öffentliches Repo** | Die Betriebsanleitung nennt Benutzernamen, Ressourcen-IDs, interne Adressen und den Host-Fingerprint, aber keine Schlüssel. Das Risiko ist wegen des VPN begrenzt. |
| **Autoscaler läuft als root** | Er muss systemd-Dienste starten und stoppen. Besser wäre ein eigener User mit eng begrenzten sudo-Rechten. |
| **Netz abweichend von der Konvention** | Unser Netz ist `10.1.0.0/24`, laut Plattform-Doku gehört team02 `10.1.2.0/24`. Funktional kein Problem, beim Neuaufbau mit IaC in Stufe 2 wird es korrigiert. |
| **Spielstand nur im Speicher** | Nach Neustart oder Absturz weg. Persistenz in einer Datenbank folgt in Stufe 2. |
| **Abhängigkeit von der externen Lobby und der Plattform** | Verfügbarkeit liegt nicht in unserer Hand. Die Plattform läuft auf einem Server und hatte am 11.11. einen Ausfall, der alle Teams betraf. |
| **Fehler im Spielcode** | `NextTreasureCardListener` übergibt `treasureCard` ohne Null-Prüfung. Die Exception wird abgefangen, das Spiel läuft weiter. |

**Nächste Schritte (Stufe 2, Abgabe 14.12.):** Infrastruktur als Code mit OpenTofu (State im S3-Bucket), Container (Branch `docker` hat bereits `Dockerfile` und `docker-compose.yml`), Deploy in der CI-Pipeline, Datenbank mit Backups für Spiele und Ergebnisse, Monitoring mit Alarm, TLS.

**Reflexion:** Viele Fehler lagen an Kleinigkeiten, die erst beim zweiten Versuch klar wurden. Die erste Test-VM war per SSH nicht erreichbar, weil der Key erst nach dem Deploy zugewiesen wurde. Die „Public IP“ ist eine private Adresse und nur über das VPN erreichbar. Der Client zeigte keine Server, weil im Feld „Server URL“ die WebSocket-Adresse statt der Lobby stand. Ein JAR war vor einem wichtigen Fix gebaut worden. Daraus nehmen wir mit, Annahmen am System nachzumessen, statt sie aus der Konfiguration abzulesen, und im Bericht sauber zu trennen zwischen dem, was unser Aufbau leistet, und dem, was die Plattform vorgibt.
