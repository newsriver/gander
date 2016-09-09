#!groovy​

node {

    stage 'checkout project'
    checkout scm

    stage 'compile'
    sh 'gradle compileJava'

    stage 'test'
    sh 'gradle test --tests com.intenthq.gander.*'

}


