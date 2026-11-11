# Team02 – Cloud-Infrastruktur Stufe 1

Stand: Nov 11, 2026 · @Romed

## Überblick

Unser Labyrinth-Spieleserver läuft seit dem 09.11.2026 (seit 11.11. als 2 bis 10 automatisch skalierte Instanzen auf den Ports 8082–8091) auf der CloudStack-VM `team02-game-01` und ist über das VPN unter `172.16.20.14:8082`–8091 erreichbar. Damit ist das technische Ziel von Stufe 1 erfüllt: Spieleserver auf einer Cloud-VM, nutzbar mit dem Game-Client.

- **Plattform:** Apache CloudStack unter [cloud.dibse.at](https://cloud.dibse.at), Projekt `team02`, Zone `zone1`
- **Steuerung:** per Web-UI oder per CloudMonkey (`cmk`) über die API
- **Belegung:** 1 von 6 VMs, 2 von 8 vCPUs, 4 von 12 GB RAM, 1 von 4 Netzwerken, 1 von 3 Public IPs
- **Termine:** Doku Stufe 1 am 23.11.2026, Präsentation am 26.11.2026

&#91;embedded content: Architektur Stufe 1 · Client, CloudStack, Lobby\]

Der Client holt die Serverliste bei der Lobby und verbindet sich dann über die Public IP. CloudStack leitet die Ports 22 und 8082 an die VM im isolierten Netz weiter.

In den folgenden Abschnitten steht zu jeder Ressource, was angelegt wurde, **wie** (mit den genauen Befehlen) und **warum**.

## Zugriff für Kollegen

Ohne VPN geht nichts: Die „Public IP“ `172.16.20.14` liegt in einem privaten Netz (RFC 1918) und ist nur über das VPN oder das Hochschulnetz erreichbar. `cloud.dibse.at` ist nur Web-UI und API, kein Zugang zu den VMs.

| Ziel | Was du brauchst | So geht's |
| --- | --- | --- |
| Spielen | VPN, Game-Client | Im Client unter „Server URL“ die Lobby `https://mgmt.dvl.spalx.dev` eintragen, dann **eine freie Instanz „Team02 Cloud / Autoscale #N“** aus der Serverliste wählen |
| Per SSH auf die VM | VPN, eigener SSH-Key, eigener User auf der VM | `ssh <user>@172.16.20.14` |
| CloudStack-Web-UI | Mitgliedschaft im Projekt `team02` | [cloud.dibse.at](https://cloud.dibse.at) → Projekt `team02` wählen |
| CloudStack per `cmk` | eigener API-Key | Web-UI → Profil → „Generate Keys“, dann `cmk set url https://cloud.dibse.at/client/api` und die eigenen Keys eintragen |

Im Feld „Server URL“ steht die **Lobby**, nicht der Spieleserver. Mit `ws://...` gibt es `MalformedURLException`, und die Serverliste bleibt leer.

### SSH-Zugang beantragen

1. Key erzeugen, am besten mit Passphrase: `ssh-keygen -t ed25519`
2. Den **öffentlichen** Key an Romed schicken. Er liegt unter Windows in `C:\Users\<user>\.ssh\id_ed25519.pub`, unter Linux/Mac in `~/.ssh/id_ed25519.pub`. Die Datei ohne `.pub` ist privat und wird nie weitergegeben.
3. Romed legt einen User an und trägt den Key ein (siehe „Benutzer und SSH-Keys“).
4. Beim ersten Login muss der Server-Fingerprint `SHA256:RH8f76cTIDcQiJEtt9PPjKVpzi4loV4GBxv+62omzYk` lauten. Dann mit `yes` bestätigen.

## Angelegte CloudStack-Ressourcen

Alles liegt im Projekt `team02` (`cd9db827-32a4-4680-a337-da9002ed7888`). Alle Befehle wurden deshalb mit `projectid=...` ausgeführt, sonst landet die Ressource im persönlichen Account.

| Ressource | Name / Wert | ID | Warum |
| --- | --- | --- | --- |
| Netzwerk | `team02-net`, Isolated, 10.1.0.0/24 | `deae537a-4da7-49d3-aaa3-6abf01912b96` | War schon vorhanden. Eigenes Netz für unsere VMs, nach außen über NAT |
| Public IP | `172.16.20.14` (Source-NAT) | `746aa0a9-7f65-4cf2-b0f8-b64795d08fc6` | Gehört zum Netz. Einzige Adresse, über die die VM von außen erreichbar ist |
| SSH-Keypair | `team02-key`, Ed25519 | `SHA256:WRR5pEx1…IJm0` | Login per Key statt Passwort. Wird beim Deploy auf die VM gespielt |
| VM | `team02-game-01`, Ubuntu 24.04, `s2.1c1g` (1 vCPU, 1 GB), seit 11.11. s2.2c4g (2 vCPU, 4 GB), IP 10.1.0.158 | `4914418f-8c82-4fd2-9dd6-75abaec33c9e` | Der Spieleserver. 1 GB reicht, weil das fertige JAR hochgeladen und nicht auf der VM gebaut wird |
| Firewall TCP 22 | 0.0.0.0/0 | `82bb5dbd-508f-49f6-9cb8-f609973def35` | SSH zur Verwaltung |
| Firewall TCP 8082–8091 | 0.0.0.0/0 | `d5de031b-892a-489d-96df-f75815967edd` | Ports der 10 Spielinstanzen (WebSocket `/game`) |
| Port-Forwarding 22 | 172.16.20.14:22 → VM:22 | `fbedff7e-945d-4586-8156-9742d2edc648` | Leitet SSH von der Public IP an die VM weiter |
| Port-Forwarding 8082–8091 | 172.16.20.14:8082–8091 → VM:8082–8091 | `07187bd7-4c9c-498b-9c50-5f91004a79a8` | Leitet die Spieler an die 10 Instanzen weiter, eine Regel für den ganzen Bereich |

Eine Firewall-Regel allein reicht nicht: Sie erlaubt den Port nur an der Public IP. Erst das Port-Forwarding leitet ihn an die VM im isolierten Netz weiter.

### Wie es angelegt wurde

1. **Keypair registrieren** (29.10.). Der Key wurde lokal erzeugt und nur der öffentliche Teil hochgeladen:

```powershell
ssh-keygen -t ed25519 -C "kelmer@team02" -f ~/.ssh/id_ed25519 -N ""
cmk register sshkeypair name=team02-key publickey="<inhalt von id_ed25519.pub>" projectid=cd9db827-32a4-4680-a337-da9002ed7888
```

2. **Firewall für SSH** (29.10.):

```powershell
cmk create firewallrule ipaddressid=746aa0a9-7f65-4cf2-b0f8-b64795d08fc6 protocol=tcp startport=22 endport=22 cidrlist=0.0.0.0/0
```

3. **VM deployen** (09.11.). Das Keypair wird direkt mitgegeben, nur so landet der Key beim ersten Boot auf der VM:

```powershell
cmk deploy virtualmachine name=team02-game-01 displayname=team02-game-01 zoneid=15f6f0a7-389e-4ee7-b4fd-5036429aac48 templateid=f512a94a-337b-4e5f-a19a-14ddf7ad28da serviceofferingid=c2296814-acaa-4057-af25-9dc3764fc7c9 networkids=deae537a-4da7-49d3-aaa3-6abf01912b96 keypair=team02-key projectid=cd9db827-32a4-4680-a337-da9002ed7888
```

4. **Ports freigeben und weiterleiten** (09.11.):

```powershell
cmk create portforwardingrule ipaddressid=746aa0a9-7f65-4cf2-b0f8-b64795d08fc6 protocol=tcp publicport=22 privateport=22 virtualmachineid=4914418f-8c82-4fd2-9dd6-75abaec33c9e openfirewall=false
cmk create firewallrule ipaddressid=746aa0a9-7f65-4cf2-b0f8-b64795d08fc6 protocol=tcp startport=8082 endport=8082 cidrlist=0.0.0.0/0
cmk create portforwardingrule ipaddressid=746aa0a9-7f65-4cf2-b0f8-b64795d08fc6 protocol=tcp publicport=8082 privateport=8082 virtualmachineid=4914418f-8c82-4fd2-9dd6-75abaec33c9e openfirewall=false
```

`openfirewall=false` bedeutet: CloudStack legt keine zusätzliche Firewall-Regel an, weil wir sie selbst verwalten.

In der Web-UI geht dasselbe unter *Instances → Add Instance* sowie *Network → team02-net → Public IP → Firewall / Port Forwarding*.

## Einrichtung auf der VM

Dieser Abschnitt beschreibt die Grundeinrichtung vom 09.11. Seit dem 11.11. ist der Einzeldienst durch automatisch skalierte Instanzen ersetzt (siehe „Bis zu 10 parallele Spielinstanzen“). Der Server lief als systemd-Dienst `labyrinth` unter einem eigenen System-User. Er startet beim Booten automatisch und wird nach einem Absturz neu gestartet.

**Die Anwendung:** `Labyrinth.Server` aus dem Repo `labyrinth` (Spring Boot 3.5.7, Java 25, WebSocket-Endpunkt `/game`, Port 8082). Beim Start meldet sich der Server bei der Lobby `https://mgmt.dvl.spalx.dev` an und schickt danach jede Sekunde ein Lebenszeichen. Der Spielstand liegt nur im Arbeitsspeicher (H2 in-memory).

| Was | Wo | Warum |
| --- | --- | --- |
| Java 25 | apt-Paket `openjdk-25-jre-headless` | Der Server braucht Java 25. Die Variante ohne grafische Oberfläche reicht auf einem Server |
| System-User `labyrinth` | ohne Login-Shell, Home `/opt/labyrinth` | Der Server läuft nicht als root. Bei einer Lücke im Spiel ist nur dieser User betroffen |
| Server-JAR | `/opt/labyrinth/labyrinth-server.jar` | Lokal gebaut (`labyrinth-server-1.0.0.jar`, Build vom 11.11.2026 inkl. Fix #74) und per `scp` hochgeladen, statt auf der VM mit 1 GB RAM zu bauen |
| Konfiguration | `/opt/labyrinth/application.properties` (0640, Owner `labyrinth`) | Name, Port und öffentliche Adresse für die Lobby |
| Dienst | `/etc/systemd/system/labyrinth.service` | Autostart, Neustart bei Absturz, Logs im Journal |

### Wie es angelegt wurde

```bash
sudo apt-get update && sudo apt-get -y upgrade
sudo apt-get -y install openjdk-25-jre-headless
sudo useradd --system --home /opt/labyrinth --shell /usr/sbin/nologin labyrinth
sudo mkdir -p /opt/labyrinth && sudo chown labyrinth: /opt/labyrinth
```

JAR vom eigenen Rechner hochladen und an seinen Platz verschieben:

```powershell
scp Labyrinth.Server\target\labyrinth-server-1.0.0.jar ubuntu@172.16.20.14:/tmp/
```

```bash
sudo mv /tmp/labyrinth-server-1.0.0.jar /opt/labyrinth/labyrinth-server.jar
sudo chown labyrinth: /opt/labyrinth/labyrinth-server.jar
```

`/opt/labyrinth/application.properties`:

```properties
spring.application.name=Labyrinth.Server
server.name=Team02 Cloud
server.port=8082
server.publichost=172.16.20.14
management.api.url=https://mgmt.dvl.spalx.dev
```

`server.publichost` muss die Public IP sein und nicht die interne 10.1.0.158. Diese Adresse meldet der Server der Lobby, und die Clients verbinden sich dann mit `ws://172.16.20.14:8082`.

`/etc/systemd/system/labyrinth.service`:

```ini
[Unit]
Description=Labyrinth Game Server
After=network-online.target
Wants=network-online.target

[Service]
User=labyrinth
WorkingDirectory=/opt/labyrinth
ExecStart=/usr/bin/java -Xmx512m -jar /opt/labyrinth/labyrinth-server.jar
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
```

```bash
sudo systemctl daemon-reload && sudo systemctl enable --now labyrinth
```

- `After=network-online.target`: Der Server startet erst, wenn das Netz da ist. Sonst schlägt die Anmeldung bei der Lobby fehl.
- `-Xmx512m`: Java darf höchstens 512 MB nutzen. Der Rest der 1 GB bleibt für Ubuntu.
- `Restart=on-failure`: Nach einem Absturz startet der Dienst nach 5 Sekunden neu.

**Geprüft am 09.11.:** Dienst `active`, Lobby meldet `Team02 Cloud | ws://172.16.20.14:8082 | LOBBY`, Verbindungstest mit dem Client erfolgreich. Am 11.11. lief der Dienst nach einem Neustart der VM wieder von selbst.

**JAR aktualisiert am 11.11.:** Das erste JAR vom 24.01.2026 war 9 Minuten vor dem Commit `fd28ee7` „fixed stale connections (#74)“ gebaut worden, der Fix fehlte also. Das JAR wurde aus dem aktuellen Repo-Stand neu gebaut und ausgetauscht. Die Lobby zeigt den Server danach wieder als `Team02 Cloud`.

So wird eine neue Version eingespielt:

```powershell
# lokal im Repo (JDK 25 in JAVA_HOME); kein "clean", OneDrive sperrt sonst target/
Labyrinth.Server\mvnw.cmd -f Labyrinth.Server\pom.xml package -DskipTests
scp Labyrinth.Server\target\labyrinth-server-1.0.0.jar <user>@172.16.20.14:/tmp/labyrinth-server-new.jar
```

```bash
# auf der VM: alte Version sichern, neue einsetzen, alle Instanzen neu starten
sudo cp -p /opt/labyrinth/labyrinth-server.jar /opt/labyrinth/labyrinth-server.jar.bak
sudo install -m 644 -o labyrinth -g labyrinth /tmp/labyrinth-server-new.jar /opt/labyrinth/labyrinth-server.jar
sudo systemctl try-restart 'labyrinth@*'
```

Zurück zur vorherigen Version: `labyrinth-server.jar.bak` zurückkopieren und alle laufenden Instanzen neu starten (sudo systemctl try-restart 'labyrinth@\*').

### Tests am 11.11.2026

Ein komplettes Spiel mit 3 Spielern lief über die Cloud-VM von der Lobby bis zum Spielende. Alle Tests liefen über das VPN mit dem neuen JAR und nach den Ubuntu-Updates.

| Zeit | Test | Ergebnis |
| --- | --- | --- |
| 19:42 | Protokolltest mit 2 Spielern (Java-WebSocket-Skript): `CONNECT`, Lobby, `DISCONNECT` | Beide bekommen `CONNECT_ACK` und sehen sich in der Lobby. Danach ist die Lobby wieder leer, ohne hängende Sitzungen |
| 19:44 | Echter Client, 1 Spieler | Serverliste über die Lobby, Verbindung zu **Team02 Cloud**, sauber getrennt |
| 19:45–19:46 | Echter Client, 3 Instanzen gleichzeitig (Test1–Test3) | Spiel mit 3 Spielern gestartet und bis `GAME_OVER` gespielt (Gewinner Test3, alle 6 Schätze). Danach alle in die Lobby zurück und sauber getrennt |

Client starten (im Repo, JDK 25 in `JAVA_HOME`): `Labyrinth.Server\mvnw.cmd -q -f Labyrinth.Client\pom.xml exec:java`. Für mehrere Spieler den Befehl mehrmals ausführen.

## Bis zu 10 parallele Spielinstanzen

Seit dem 11.11.2026 können auf der VM bis zu 10 Spieleserver nebeneinander laufen, „Team02 Cloud / Autoscale #1“ bis „#10“ auf den Ports 8082 bis 8091. Damit sind bis zu 10 Spiele gleichzeitig möglich, also bis zu 40 Spieler. Wie viele davon gerade laufen, entscheidet der Autoscaler (siehe unten).

**Warum so:** Der Server verwaltet pro Prozess genau ein Spiel (ein `Game` im Singleton `GameService`, keine Raum-ID im Protokoll). Mehrere Prozesse brauchen keine Code-Änderung und bleiben mit den Clients aller Gruppen kompatibel. Mehrere Spiele in einem Prozess hätten tagelangen Umbau bedeutet und das gemeinsame Protokoll gefährdet.

**Für Spieler ändert sich nichts:** Jede Instanz meldet sich selbst mit Namen und Port bei der Lobby an. Im Client wählt man einfach „Team02 Cloud / Autoscale #N“ aus der Liste und muss keinen Port kennen.

| Was | Wert | Warum |
| --- | --- | --- |
| VM-Größe | `s2.2c4g` (2 vCPU, 4 GB), vorher `s2.1c1g` | Eine Instanz braucht im Leerlauf ca. 180–230 MB. 10 Instanzen belegen zusammen ca. 1,9 GB |
| Speicher pro Instanz | `-Xmx256m` | Genügend für ein Spiel, alle 10 passen sicher in 4 GB |
| Ports | 8082–8091, eine Firewall- und eine Port-Forwarding-Regel für den ganzen Bereich | Neue Instanzen bis #10 brauchen keine Änderung in CloudStack |
| Dienst | systemd-Vorlage `labyrinth@.service`, Instanz `labyrinth@N` = Port 8081+N | Ein Dienst-Muster für alle Instanzen, einzeln start- und stoppbar |
| Projekt-Belegung | 1/6 VMs, 2/8 vCPUs, 4/12 GB RAM | Genug Reserve für Stufe 2 und 3 |

100 Instanzen wurden geprüft und verworfen: Sie bräuchten ca. 20 GB RAM (Projektlimit 12 GB), und die gemeinsame Lobby wäre für alle Gruppen mit 100 Einträgen überladen.

### Wie es angelegt wurde

1. **VM vergrößern.** Das geht nur im gestoppten Zustand:

```powershell
cmk stop virtualmachine id=4914418f-8c82-4fd2-9dd6-75abaec33c9e
cmk change serviceforvirtualmachine id=4914418f-8c82-4fd2-9dd6-75abaec33c9e serviceofferingid=29eece06-ba26-465b-83c6-e023e1e1ac69
cmk start virtualmachine id=4914418f-8c82-4fd2-9dd6-75abaec33c9e
```

2. **Port 8082 durch den Bereich 8082–8091 ersetzen.** CloudStack erlaubt keine überlappenden Regeln, deshalb wurden die alten Regeln zuerst gelöscht:

```powershell
cmk delete portforwardingrule id=35d8d0c4-e30b-414e-a836-2f120824b54f
cmk delete firewallrule id=165e58f5-4507-42f4-a0b6-042025ad32d5
cmk create firewallrule ipaddressid=746aa0a9-7f65-4cf2-b0f8-b64795d08fc6 protocol=tcp startport=8082 endport=8091 cidrlist=0.0.0.0/0
cmk create portforwardingrule ipaddressid=746aa0a9-7f65-4cf2-b0f8-b64795d08fc6 protocol=tcp publicport=8082 publicendport=8091 privateport=8082 privateendport=8091 virtualmachineid=4914418f-8c82-4fd2-9dd6-75abaec33c9e openfirewall=false
```

3. **systemd-Vorlage** `/etc/systemd/system/labyrinth@.service` auf der VM:

```ini
[Unit]
Description=Labyrinth Game Server #%i
After=network-online.target
Wants=network-online.target

[Service]
User=labyrinth
WorkingDirectory=/opt/labyrinth
ExecStart=/bin/sh -c 'exec /usr/bin/java -Xmx256m -jar /opt/labyrinth/labyrinth-server.jar --server.port=$((8081 + %i)) "--server.name=Team02 Cloud / Autoscale #%i"'
Restart=on-failure
RestartSec=5

[Install]
WantedBy=multi-user.target
```

`%i` ist die Instanznummer. Port und Name werden beim Start übergeben und überschreiben die Werte aus `application.properties`.

4. **Alten Einzeldienst abschalten, 10 Instanzen starten:**

```bash
sudo systemctl disable --now labyrinth
sudo systemctl daemon-reload
for i in $(seq 1 10); do sudo systemctl enable --now labyrinth@$i; sleep 5; done
```

`enable` sorgt dafür, dass die Instanzen nach einem Neustart der VM von selbst wieder hochkommen. Seit es den Autoscaler gibt, sind nur noch #1 und #2 enabled, den Rest startet er bei Bedarf. Die alte `labyrinth.service` liegt noch auf der VM und ist nur abgeschaltet.

### Instanzen bedienen

| Zweck | Befehl |
| --- | --- |
| Status aller Instanzen | `systemctl list-units 'labyrinth@*'` |
| Eine Instanz starten / stoppen | `sudo systemctl start labyrinth@3` / `sudo systemctl stop labyrinth@3` |
| Alle neu starten (z. B. nach neuem JAR) | `sudo systemctl try-restart 'labyrinth@*'` |
| Logs einer Instanz | `journalctl -u labyrinth@3 -f` |
| Instanz dauerhaft abschalten | `sudo systemctl disable --now labyrinth@10` |
| Zurück zum Einzelserver | `sudo systemctl disable --now 'labyrinth@*'` und `sudo systemctl enable --now labyrinth` |

**Geprüft am 11.11.:** alle 10 Dienste `active`, alle 10 Ports über das VPN erreichbar, die Lobby zeigt „Team02 Cloud #1“ bis „#10“ (damals noch ohne „/ Autoscale“) mit Status `LOBBY`.

### Autoscaler

Seit dem 11.11.2026 laufen nicht mehr dauerhaft alle 10 Instanzen. Ein Autoscaler hält immer 2 leere Instanzen bereit und startet oder stoppt die übrigen nach Bedarf. Im Leerlauf laufen nur #1 und #2 (ca. 0,4 GB statt 1,9 GB RAM).

**Warum:** Leere Instanzen brauchen kaum CPU, belegen aber je ca. 190 MB RAM. Statische Kapazität verschwendet also Speicher. Der Autoscaler ist ein erster Schritt Richtung Stufe 3 (Skalierung).

| Lage | Aktion |
| --- | --- |
| Weniger als 2 freie Instanzen (`LOBBY`, 0 Spieler) | fehlende Instanzen auf einmal starten, höchstens bis #10 |
| Mehr als 2 freie, davon welche seit 5 min leer | alle überzähligen leeren Instanzen auf einmal stoppen, mindestens 2 bleiben |
| Lobby nicht erreichbar | nichts tun |

Ein Lauf erledigt alles auf einmal: Fehlen zum Beispiel 2 freie Instanzen, startet er 2 (niedrigste Nummern zuerst). Sind 3 überzählige seit 5 min leer, stoppt er alle 3 (höchste Nummern zuerst). Eine Instanz, die gerade startet und noch nicht in der Lobby steht, zählt als frei, damit nicht doppelt gestartet wird. Gestoppt werden nur leere Instanzen, laufende Spiele bleiben unberührt.

**Woher kommen die Daten?** Der Server hat keine eigene Status-Schnittstelle. Der Autoscaler liest deshalb `https://mgmt.dvl.spalx.dev/servers`. Dort meldet jede Instanz jede Sekunde Status und Spielerzahl.

| Datei auf der VM | Zweck |
| --- | --- |
| `/usr/local/bin/labyrinth-autoscaler.py` | Logik (Python 3, nur Standardbibliothek). Grenzwerte oben im Skript: `MIN_FREE=2`, `MIN_RUNNING=2`, `MAX_INSTANCES=10`, `IDLE_BEFORE_STOP=300` |
| `/etc/systemd/system/labyrinth-autoscaler.service` | führt das Skript einmal aus (`Type=oneshot`, als root wegen `systemctl`) |
| `/etc/systemd/system/labyrinth-autoscaler.timer` | startet den Lauf 60 s nach dem Booten und dann alle 30 s |
| `/run/labyrinth-autoscaler/idle-N` | merkt sich, seit wann Instanz N leer ist. Liegt im Arbeitsspeicher (tmpfs) und ist nach jedem Neustart leer, damit kein alter Zeitstempel eine frisch gestartete Instanz sofort stoppt |

**Installiert mit:**

```bash
sudo install -m 755 /tmp/labyrinth-autoscaler.py /usr/local/bin/
sudo install -m 644 /tmp/labyrinth-autoscaler.service /tmp/labyrinth-autoscaler.timer /etc/systemd/system/
sudo systemctl daemon-reload
for i in 3 4 5 6 7 8 9 10; do sudo systemctl disable labyrinth@$i; done   # beim Booten nur #1 und #2
sudo systemctl enable --now labyrinth-autoscaler.timer
```

**Bedienen:** Entscheidungen ansehen mit `journalctl -u labyrinth-autoscaler -f`. Nächster Lauf: `systemctl list-timers labyrinth-autoscaler.timer`. Abschalten mit `sudo systemctl disable --now labyrinth-autoscaler.timer`, danach bleiben die Instanzen so, wie sie gerade sind.

**Grenze:** Der Autoscaler skaliert nur innerhalb dieser einen VM, also bis 10 Instanzen. Mehrere VMs oder Container folgen in Stufe 3.

**Name in der Lobby:** Seit dem 11.11. heißen die Instanzen „Team02 Cloud / Autoscale #N“. Der Name steht in der Vorlage `labyrinth@.service`. Laufende Instanzen übernehmen eine Änderung erst nach `sudo systemctl try-restart 'labyrinth@*'`, gestoppte beim nächsten Start.

**Achtung:** Immer `try-restart` statt `restart` verwenden. `restart 'labyrinth@*'` würde auch die vom Autoscaler gestoppten Instanzen wieder starten.

**Getestet am 11.11.2026:**

| Zeit | Ereignis | Reaktion des Autoscalers |
| --- | --- | --- |
| 20:11–20:13 | 10 Instanzen laufen, alle seit 5 min leer, Testspieler auf #1 und #2 | stoppt #10, #9, #8 (damals noch eine pro Lauf) |
| 20:15 | Neustart der VM über CloudStack | beim Booten starten nur #1 und #2, Autoscaler läuft 60 s danach |
| 20:16 | ein Spieler verbindet sich mit #1, nur noch 1 Instanz frei | startet um 20:16:45 #3, wieder 2 frei |

## Benutzer und SSH-Keys

Jede Person bekommt einen eigenen User mit eigenem Key. So sieht man, wer was gemacht hat, und kann einen Zugang einzeln wieder entziehen. Login per Passwort ist auf der VM abgeschaltet (`PasswordAuthentication no`).

| User | Key | sudo | Warum |
| --- | --- | --- | --- |
| `ubuntu` | `team02-key` | ja (cloud-init) | Standarduser des Ubuntu-Images, bleibt als Notfallzugang |
| `romed` | `team02-key` | ja, ohne Passwort (`/etc/sudoers.d/90-romed`) | Eigener Admin-User für Romed, angelegt am 11.11. |
| `thomas` | eigener Ed25519-Key (`SHA256:afP4j9…X5I`, `bracketz-pc`) | ja, ohne Passwort (`/etc/sudoers.d/90-thomas`) | Eigener Admin-User für Thomas, angelegt am 11.11. |

Die neuen User haben kein Passwort. sudo läuft deshalb ohne Passwortabfrage. Geschützt ist der Zugang trotzdem, denn hereinkommen kann nur, wer den privaten Key hat.

### So wird ein neuer User angelegt

Beispiel für `thomas`. Die Befehle laufen auf der VM als `ubuntu` oder `romed`:

```bash
sudo adduser --disabled-password --gecos "Thomas" thomas
sudo install -d -m 700 -o thomas -g thomas /home/thomas/.ssh
echo 'ssh-ed25519 AAAA... bracketz-pc' | sudo tee /home/thomas/.ssh/authorized_keys
sudo chown thomas:thomas /home/thomas/.ssh/authorized_keys
sudo chmod 600 /home/thomas/.ssh/authorized_keys
```

Nur wenn die Person Admin-Rechte braucht:

```bash
sudo usermod -aG sudo thomas
echo 'thomas ALL=(ALL) NOPASSWD:ALL' | sudo tee /etc/sudoers.d/90-thomas
sudo chmod 440 /etc/sudoers.d/90-thomas
sudo visudo -cf /etc/sudoers.d/90-thomas
```

- `--disabled-password`: Der User hat kein Passwort und kann sich nur per Key anmelden.
- Rechte `700` / `600`: SSH ignoriert `authorized_keys`, wenn andere die Datei lesen oder ändern können.
- `visudo -cf`: prüft die sudo-Datei. Ein Tippfehler darin kann sudo für alle lahmlegen.
- Prüfen: `sudo -l -U thomas` zeigt die sudo-Rechte, `sudo ssh-keygen -lf /home/thomas/.ssh/authorized_keys` den Fingerprint des Keys.

**Zugang entziehen:** `sudo deluser --remove-home <user>` und `sudo rm /etc/sudoers.d/90-<user>`.

## Sicherheit und offene Punkte

Die VM ist nur über das VPN erreichbar, nur die Ports 22 und 8082 sind offen, und SSH lässt nur Keys zu. Für Stufe 1 reicht das. Die folgenden Punkte sind noch offen.

**Umgesetzt:**

- SSH nur per Key, kein Passwort-Login, kein root-Login mit Passwort
- Spieleserver läuft als eigener User ohne Login-Shell, nicht als root
- Ein eigener User pro Person, Zugang einzeln entziehbar
- Private Keys bleiben bei ihren Besitzern, nur `.pub`-Dateien werden weitergegeben

**Offen:**

- [ ] `team02-key` hat keine Passphrase. Eine Passphrase setzen mit `ssh-keygen -p -f ~/.ssh/id_ed25519`
- [ ] Notfall-Passwort für `ubuntu` setzen (`sudo passwd ubuntu`), damit „View Console“ in der Web-UI funktioniert, falls SSH ausfällt
- [ ] Firewall-Regeln von `0.0.0.0/0` auf VPN- und Hochschulnetz einschränken (Stufe 2)
- [x] JAR auf den aktuellen Code-Stand bringen (erledigt am 11.11., Fix #74 jetzt enthalten)
- [x] Test mit mehreren Spielern gleichzeitig (erledigt am 11.11.: ganzes Spiel mit 3 Spielern)
- [ ] Fehler im Spielcode beheben: `NextTreasureCardListener` wirft gegen Spielende eine `NullPointerException` (`treasureCard` ist null), wahrscheinlich sobald ein Spieler alle Schätze hat. Das Spiel läuft trotzdem weiter. Behebung: Prüfung auf `null` vor dem Versand
- [ ] Bei der Q&A klären, was „über Web erreichbar“ meint, denn ein Web-Frontend gibt es im Repo nicht
- [ ] Für die Präsentation müssen alle Clients im VPN sein
- [ ] Spielstand liegt nur im Speicher und ist nach einem Neustart weg. Persistenter Speicher kommt in Stufe 2
- [x] Ubuntu-Updates einspielen, erledigt am 11.11.: 8 Pakete inkl. Kernel 6.8.0-149, danach Neustart (`sudo apt-get update && sudo apt-get upgrade`)

## Wichtige Befehle

Auf der VM (nach `ssh <user>@172.16.20.14`):

| Zweck | Befehl |
| --- | --- |
| Laufen alle Instanzen? | `systemctl list-units 'labyrinth@*'` |
| Logs einer Instanz live | `journalctl -u labyrinth@1 -f` |
| Alle Instanzen neu starten | `sudo systemctl try-restart 'labyrinth@*'` |
| Lauschen die Ports? | `sudo ss -tlnp \| grep java` |
| Speicher prüfen | `free -m` |
| Neues JAR einspielen | siehe „JAR aktualisiert“, danach `sudo systemctl try-restart 'labyrinth@*'` |
| Wer ist eingeloggt? | `who` |

Vom eigenen Rechner mit VPN:

| Zweck | Befehl |
| --- | --- |
| Port erreichbar? (Windows) | `Test-NetConnection 172.16.20.14 -Port 8082` |
| VM-Status in CloudStack | `cmk list virtualmachines projectid=cd9db827-32a4-4680-a337-da9002ed7888 filter=name,state,ipaddress` |
| Ist der Server in der Lobby? | `https://mgmt.dvl.spalx.dev/servers` im Browser öffnen |
| VM über CloudStack neu starten | `cmk reboot virtualmachine id=4914418f-8c82-4fd2-9dd6-75abaec33c9e` |

## Lessons Learned

Die wichtigste Lehre: Das Keypair muss **beim Deploy** mitgegeben werden. Daran ist unsere erste VM gescheitert.

| Problem | Ursache | Lösung |
| --- | --- | --- |
| Erste VM `team02-test-01` (29.10.) nicht per SSH erreichbar, `Permission denied (publickey)` | Ohne Keypair deployt, der Key wurde erst danach mit `reset sshkeyforvirtualmachine` zugewiesen. cloud-init übernimmt Keys aber nur beim ersten Boot | Am 09.11. gelöscht und als `team02-game-01` mit `keypair=team02-key` neu deployt |
| SSH auf `172.16.20.14` läuft in einen Timeout | Die „Public IPs“ liegen in einem privaten Netz, das nur intern erreichbar ist | VPN verwenden |
| `Service offering is inactive` beim Deploy | Die `s1.*`-Offerings wurden deaktiviert | Nur noch `s2.*` verwenden, z. B. `s2.1c1g` |
| `cmk`-Fehler `failed to detect signature algorithm` / HTTP 401 | Platzhalter statt der echten API-Keys eingetragen | Die echten Keys (86 Zeichen) aus der Web-UI eintragen |
| Client zeigt keine Server, `MalformedURLException: unknown protocol: ws` | `ws://...` im Feld „Server URL“ eingetragen | Dort gehört die Lobby `https://mgmt.dvl.spalx.dev` hin |
| OpenSSH lehnt den privaten Key unter Windows ab | Andere Benutzer durften die Datei lesen | Rechte einschränken mit `icacls <key> /inheritance:r /grant:r "%USERNAME%:R"` |
| `Host key verification failed` | Erste Verbindung ohne Möglichkeit, `yes` einzugeben | Einmal interaktiv verbinden und den Fingerprint mit `yes` bestätigen |
