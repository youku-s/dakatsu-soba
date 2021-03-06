'use strict';

var aws = require('aws-sdk');
const {table} = require('table');

exports.handler = function (event, context, callback) {
    var emptyToSpace = function(s) {
        return s === '' ? ' ' : s;  
    };
    var json = JSON.parse(decodeURIComponent(event.queryParameters.query));
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

            bodyLeft: ``,
            bodyRight: ``,
            bodyJoin: `:`,

            joinBody: ``,
            joinLeft: ``,
            joinRight: ``,
            joinJoin: ``
        },
    };

    var output = '';
    output += '■ パーソナル' + '\r\n';
    output += 'キャラクター名：' + json.profile.name + '\r\n';
    output += '種族：' + json.profile.race + '\r\n';
    output += '享年：' + json.profile.age + '\r\n';
    output += '暗示：' + json.profile.implication + '\r\n';
    output += '身長：' + json.profile.height + '\t体重：' + json.profile.weight + '\r\n';
    output += '髪の色：' + json.profile.hair + '\t瞳の色：' + json.profile.eye + '\t肌の色：' + json.profile.skin + '\r\n';
    var acts = json.tabs.reduce(function(acc1, tab) {
        if (tab.tabType == 'ManeuvaTab') {
            return acc1 + t.items.reduce(function(acc2, item) {
                return acc2 + (item.act || 0);
            });
        } else {
            return acc1;
        }
    }) + 6;
    output += '最大行動値：' + acts + '\r\n';
    output += '\r\n';

    output += '■ 記憶のカケラ' + '\r\n';
    var memories = [];
    memories.push(['名前', '詳細']);
    json.profile.memories.forEach(m => {
        memories.push([emptyToSpace(m.name), emptyToSpace(m.description)]);
    });
    output += table(memories, config).replace(/\n\n/gi, '\n');
    output += '\r\n';

    output += '■ 未練' + '\r\n';
    var regrets = [];
    regrets.push(['対象', '種類', '現在値', '最大値', '発狂効果', '備考']);
    json.profile.regrets.forEach(r => {
        regrets.push([emptyToSpace(r.target), emptyToSpace(r.name), emptyToSpace(r.currentVal), emptyToSpace(r.maxVal), emptyToSpace(r.negative), emptyToSpace(r.description)]);
    });
    output += table(regrets, config).replace(/\n\n/gi, '\n');
    output += '\r\n';

    output += '■ カルマ' + '\r\n';
    var karmas = [];
    karmas.push(['条件', '詳細']);
    json.profile.karmas.forEach(k => {
        karmas.push([emptyToSpace(k.name), emptyToSpace(k.description)]);
    });
    output += table(karmas, config).replace(/\n\n/gi, '\n');
    output += '\r\n';

    output += '■ メモ' + '\r\n';
    output += json.profile.memo + '\r\n';
    output += '\r\n';

    output += '■ ポジション' + '\r\n';
    output += 'ポジション：' + json.classes.positions.map(p => { return p.name }).join('/') + '\r\n';
    output += 'サブポジション：' + json.classes.subPositions.map(p => { return p.name }).join('/') + '\r\n';
    output += 'ハイテック：' + json.classes.highTechs.map(p => { return p.name }).join('/') + '\r\n';
    output += '\r\n';

    output += '■ クラス' + '\r\n';
    var classes = [];
    classes.push(['種別', '取得元', 'クラス名', '個数']);
    json.classes.classes.forEach(c => {
        classes.push([emptyToSpace(c.category), emptyToSpace(c.from), emptyToSpace(c.name), emptyToSpace(c.number)]);
    });
    output += table(classes, config).replace(/\n\n/gi, '\n');
    output += '\r\n';

    output += '■ 強化点' + '\r\n';
    var points = [];
    var busou = 0;
    var heni = 0;
    var kaizou = 0;
    points.push(['クラス名', '武装', '変異', '改造']);
    json.classes.points.forEach(p => {
        busou += p.busou;
        heni += p.heni;
        kaizou += p.kaizou;
        points.push([emptyToSpace(p.name), p.busou, p.heni, p.kaizou]);
    });
    points.push(['総計', busou, heni, kaizou]);
    output += table(points, config).replace(/\n\n/gi, '\n');
    output += '\r\n';

    json.tabs.forEach(t => {
        output += '■ ' + t.title + '\r\n';
        if (t.tabType == 'ManeuvaTab') {
            var maneuvas = [];
            maneuvas.push(['悪意', '部位', 'カテゴリー', '種別', 'マニューバ', 'タイミング', 'コスト', '射程', '効果']);
            t.items.forEach(i => {
                maneuvas.push([i.malice, emptyToSpace(i.region), emptyToSpace(i.maneuvaType), emptyToSpace(i.category), emptyToSpace(i.name), emptyToSpace(i.timing), emptyToSpace(i.cost), emptyToSpace(i.range), emptyToSpace(i.description)]);
            });
            output += table(maneuvas, config).replace(/\n\n/gi, '\n');
            output += '\r\n';
        } else if (t.tabType == 'ResourceTab') {
            var resources = [];
            resources.push(['名前', '説明']);
            t.items.forEach(i => {
                resources.push([emptyToSpace(i.name), emptyToSpace(i.description)]);
            });
            output += table(resources, config).replace(/\n\n/gi, '\n');
            output += '\r\n';
        }
    });

    callback(null, output);
};