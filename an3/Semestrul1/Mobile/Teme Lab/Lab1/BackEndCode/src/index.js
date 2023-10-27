const Koa = require('koa');
const app = new Koa();
const server = require('http').createServer(app.callback());
const WebSocket = require('ws');
const wss = new WebSocket.Server({ server });
const Router = require('koa-router');
const cors = require('koa-cors');
const bodyparser = require('koa-bodyparser');

app.use(bodyparser());
app.use(cors());
app.use(async(ctx, next) => {
    const start = new Date();
    await next();
    const ms = new Date() - start;
    console.log(`${ctx.method} ${ctx.url} ${ctx.response.status} - ${ms}ms`);
});

app.use(async(ctx, next) => {
    await new Promise(resolve => setTimeout(resolve, 2000));
    await next();
});

app.use(async(ctx, next) => {
    try {
        await next();
    } catch (err) {
        ctx.response.body = { issue: [{ error: err.message || 'Unexpected error' }] };
        ctx.response.status = 500;
    }
});

class Restaurant {
    constructor(id, name, description, closed) {
        this.id = id;
        this.name = name;
        this.description = description;
        this.date = new Date();
        this.closed = closed;
    }
}

const restaurants = [];
restaurants.push(new Restaurant(0, 'Pizza Club', 'pizza', false));
let lastId = restaurants[restaurants.length - 1].id;
const pageSize = 10;

const broadcast = data =>
    wss.clients.forEach(client => {
        if (client.readyState === WebSocket.OPEN) {
            client.send(JSON.stringify(data));
        }
    });

const router = new Router();

router.get('/item', ctx => {
    ctx.response.body = restaurants;
    ctx.response.status = 200;
});

router.get('/item/:id', async(ctx) => {
    const itemId = ctx.request.params.id;
    const item = restaurants.find(item => itemId === item.id);
    if (item) {
        ctx.response.body = item;
        ctx.response.status = 200; // ok
    } else {
        ctx.response.body = { message: `item with id ${itemId} not found` };
        ctx.response.status = 404; // NOT FOUND (if you know the resource was deleted, then return 410 GONE)
    }
});

const createItem = async(ctx) => {
    const item = ctx.request.body;
    if (!item.name) { // validation
        ctx.response.body = { message: 'Name is missing' };
        ctx.response.status = 400; //  BAD REQUEST
        console.log('Name is missing')
        return;
    }
    // if (!item.description) { // validation
    //     ctx.response.body = { message: 'Description is missing' };
    //     ctx.response.status = 400; //  BAD REQUEST
    //     console.log('Description is missing')
    //     return;
    // }

    item.id = `${parseInt(lastId) + 1}`;
    lastId = item.id;
    restaurants.push(item);
    ctx.response.body = item;
    ctx.response.status = 201; // CREATED
    broadcast({ event: 'created', payload: { item } });
};

router.post('/item', async(ctx) => {
    await createItem(ctx);
});

router.put('/item/:id', async(ctx) => {
    const id = ctx.params.id;
    const item = ctx.request.body;
    const itemId = item.id;
    if (itemId && id !== item.id) {
        ctx.response.body = { message: `Param id and body id should be the same` };
        ctx.response.status = 400; // BAD REQUEST
        return;
    }
    if (!itemId) {
        await createItem(ctx);
        return;
    }
    const index = restaurants.findIndex(item => item.id === id);
    if (index === -1) {
        ctx.response.body = { issue: [{ error: `item with id ${id} not found` }] };
        ctx.response.status = 400; // BAD REQUEST
        return;
    }

    restaurants[index] = item;
    ctx.response.body = item;
    ctx.response.status = 200; // OK
    broadcast({ event: 'updated', payload: { item } });
});

router.del('/item/:id', ctx => {
    const id = ctx.params.id;
    const index = restaurants.findIndex(item => id === item.id);
    if (index !== -1) {
        const item = restaurants[index];
        restaurants.splice(index, 1);
        broadcast({ event: 'deleted', payload: { item } });
    }
    ctx.response.status = 204; // no content
});

// setInterval(() => {
//     // lastUpdated = new Date();
//     // lastId = `${parseInt(lastId) + 1}`;
//     // const item = new Restaurant({ id: lastId, text: `item ${lastId}`, date: lastUpdated, version: 1 });
//     // restaurants.push(item);
//     // console.log(`New item: ${item.text}`);
//     // broadcast({ event: 'created', payload: { item } });
//     console.log(restaurants)
// }, 5000);

app.use(router.routes());
app.use(router.allowedMethods());

server.listen(3000);