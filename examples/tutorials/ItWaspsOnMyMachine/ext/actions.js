export const saveExcuse = async (excuse, context) => {
    console.log(excuse.text);

    var text = excuse.text;
    return context.entities.Excuse.create({
        data: {text}
      })
}