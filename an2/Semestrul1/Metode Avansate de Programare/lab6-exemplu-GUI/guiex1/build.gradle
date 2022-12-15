plugins {
    id 'java'
    id 'application'
    id 'org.openjfx.javafxplugin' version '0.0.13'
    id 'org.beryx.jlink' version '2.25.0'
}

group 'com.example'
version '1.0-SNAPSHOT'

repositories {
    mavenCentral()
}

ext {
    junitVersion = '5.8.2'
}

sourceCompatibility = '17'
targetCompatibility = '17'

tasks.withType(JavaCompile) {
    options.encoding = 'UTF-8'
}

application {
    mainModule = 'com.example.guiex1'
    mainClass = 'com.example.guiex1.HelloApplication'
}

javafx {
    version = '17.0.2'
    modules = ['javafx.controls', 'javafx.fxml']
}

dependencies {
    implementation('org.controlsfx:controlsfx:11.1.1')
    implementation('com.dlsc.formsfx:formsfx-core:11.5.0') {
        exclude(group: 'org.openjfx')
    }
    implementation('org.kordamp.bootstrapfx:bootstrapfx-core:0.4.0')

    testImplementation("org.junit.jupiter:junit-jupiter-api:${junitVersion}")
    testRuntimeOnly("org.junit.jupiter:junit-jupiter-engine:${junitVersion}")

    // https://mvnrepository.com/artifact/org.postgresql/postgresql
    implementation group: 'org.postgresql', name: 'postgresql', version: '42.5.0'
}

test {
    useJUnitPlatform()
}

jlink {
    imageZip = project.file("${buildDir}/distributions/app-${javafx.platform.classifier}.zip")
    options = ['--strip-debug', '--compress', '2', '--no-header-files', '--no-man-pages']
    launcher {
        name = 'app'
    }
}

jlinkZip {
    group = 'distribution'
}