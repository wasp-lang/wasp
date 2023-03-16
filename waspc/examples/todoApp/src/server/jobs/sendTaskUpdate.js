import { emailSender } from '@wasp/email/index.js'

export async function sendTaskUpdateFn(args, context) {
    const { username, taskId, isDone } = args;
    const task = await context.entities.Task.findUnique({ where: { id: taskId } })
    try {
        const info = await emailSender.send({
            to: process.env.ADMIN_EMAIL,
            subject: 'Task status changed',
            text: `User ${username} says "${task.description}" is ${
            isDone ? 'done' : 'not done'
            }.`,
            html: getHtml({ username, task, isDone }),
        })
        console.log('Email sent: ', info)
        return { success: true }
    } catch (e) {
        console.log('Error while sending email: ', e)
        return { success: false }
    }
}

function getHtml({ username, task, isDone }) {
    return `<div style="font-size: 18px">${
        username
    } says task <strong>${task.description}</strong> is ${
        isDone
        ? '<span style="color: lime">done</span>'
        : '<span style="color: tomato">not done</span>'
    }.</div>`
}
  