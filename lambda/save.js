'use strict';

var aws = require('aws-sdk');
var bcrypt = require('bcryptjs');
var docClient = new aws.DynamoDB.DocumentClient({region: 'ap-northeast-1'});

exports.handler = function (event, context, callback) {
    if (event.uuid === null || event.uuid === undefined) {
        callback(new Error("uuidは必須です。"));
    }

    const queryParams = {
        TableName: 'dakatsu-sheet',
        FilterExpression : '#u = :val',
        ExpressionAttributeValues : {':val': event['uuid']},
        ExpressionAttributeNames : {'#u': 'uuid'},
    };

    docClient.scan(queryParams, (error, data) => {
        if (error) {
            callback(new Error("取得に失敗しました。"));
        } else if (data.Count < 1) {
            // 新規登録ケース
            // パスワードが設定されている場合は、ハッシュ化する
            if (event.password !== null && event.password !== "") {
                bcrypt.genSalt(10, function(err, salt) {
                    bcrypt.hash(event.password, salt, function(err, hash) {
                        event.password = hash;
                    
                        var putParams = {
                            TableName: 'dakatsu-sheet',
                            Item: event
                        };
                        docClient.put(putParams, function (err, data) {
                            if (err) {
                                callback(err);
                            }
                            context.succeed({
                                "statusCode": 200,
                                "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                                "body": ""
                            });
                        });
                    });
                });
            } else {
                var putParams = {
                    TableName: 'dakatsu-sheet',
                    Item: event
                };
                docClient.put(putParams, function (err, data) {
                    if (err) {
                        callback(err);
                    }
                    context.succeed({
                        "statusCode": 200,
                        "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                        "body": ""
                    });
                });
            }
        } else {
            // 更新ケース
            if (data[0].password !== null && data[0].password !== "") {
                
                bcrypt.compare(event.passoword, data[0].password).then((res) => {
                    if (res) {
                        var putParams = {
                            TableName: 'dakatsu-sheet',
                            Item: event
                        };
                        docClient.put(putParams, function (err, data) {
                            if (err) {
                                context.fail(err);
                            }
                            context.succeed({
                                "statusCode": 200,
                                "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                                "body": ""
                            });
                        });
                    } else {
                        callback(new Error("パスワードが一致しません。"));
                    }
                });
                
            } else {
                var putParams = {
                    TableName: 'dakatsu-sheet',
                    Item: event
                };
                docClient.put(putParams, function (err, data) {
                    if (err) {
                        context.fail(err);
                    }
                    context.succeed({
                        "statusCode": 200,
                        "headers": {'Content-Type': 'application/json', 'Access-Control-Allow-Origin': '*'},
                        "body": ""
                    });
                });
            }
        }
    });
};