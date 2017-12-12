'use strict';

var aws = require('aws-sdk');
var table = require('table');

exports.handler = function (event, context, callback) {

    var config = {
        border: {
            topBody: ``,
            topJoin: ``,
            topLeft: ``,
            topRight: ``,
     
            bottomBody: ``,
            bottomJoin: ``,
            bottomLeft: ``,
            bottomRight: ``,
     
            bodyLeft: `│`,
            bodyRight: `│`,
            bodyJoin: `│`,
     
            joinBody: ``,
            joinLeft: ``,
            joinRight: ``,
            joinJoin: ``
        }
    };
    
    var output = '';
    output = output + '■ パーソナル' + '\r\n';
    output = output + 'キャラクター名：' + event.profile.name + '\r\n';
    output = output + '種族：' + event.profile.race + '\r\n';
    output = output + '享年：' + event.profile.age + '\r\n';
    output = output + '身長：' + event.profile.height + '\t体重：' + event.profile.weight + '\r\n';
    output = output + '髪の色：' + event.profile.hair + '\t瞳の色：' + event.profile.eye + '\t肌の色：' + event.profile.skin + '\r\n';
    output = output + '\r\n';

    output = output + '■ 記憶のカケラ' + '\r\n';
    var memories = [];
    memories.push(['名前', '詳細']);
    event.profile.memories.forEach(m => {
        memories.push([m.name, m.description]);
    });
    output = output + table(memories, config);
    output = output + '\r\n';

    output = output + '■ 未練' + '\r\n';
    var regrets = [];
    regrets.push(['対象', '種類', '現在値', '最大値', '発狂効果', '備考']);
    event.profile.regrets.forEach(r => {
        regrets.push([r.target, r.name, r.currentVal, r.maxVal, r.negative, r.description]);
    });
    output = output + table(regrets, config);
    output = output + '\r\n';
    
    output = output + '■ カルマ' + '\r\n';
    var karmas = [];
    karmas.push(['条件', '詳細']);
    event.profile.karmas.forEach(k => {
        karmas.push([k.name, k.description]);
    });
    output = output + table(karmas, config);
    output = output + '\r\n';
    
    output = output + '■ メモ' + '\r\n';
    output = output + event.profile.memo + '\r\n';
    output = output + '\r\n';
    
    callback(null, output);
};