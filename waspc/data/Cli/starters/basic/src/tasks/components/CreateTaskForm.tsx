import React from "react";
import { Controller, SubmitHandler, useForm } from "react-hook-form";
import { createTask, getTags, useQuery } from "wasp/client/operations";
import { Tag } from "wasp/entities";
import { Button } from "../../shared/components/Button";
import { Input } from "../../shared/components/Input";
import { CreateTagDialog } from "../../tags/components/CreateTagDialog";
import { TagLabel } from "../../tags/components/TagLabel";

interface CreateTaskFormValues {
  description: string;
  tagIds: string[];
}

export function CreateTaskForm() {
  const { data: tags } = useQuery(getTags);
  const { handleSubmit, getValues, setValue, watch, control, reset } =
    useForm<CreateTaskFormValues>({
      defaultValues: {
        description: "",
        tagIds: [],
      },
    });

  const onSubmit: SubmitHandler<CreateTaskFormValues> = async (data, event) => {
    event?.stopPropagation();

    try {
      await createTask(data);
    } catch (err: unknown) {
      window.alert(`Error while creating task: ${String(err)}`);
    } finally {
      reset();
    }
  };

  const toggleTag = React.useCallback(
    function toggleTag(id: Tag["id"]) {
      const tagIds = getValues("tagIds");
      if (tagIds.includes(id)) {
        setValue(
          "tagIds",
          tagIds.filter((tagId) => tagId !== id),
        );
      } else {
        setValue("tagIds", [...tagIds, id]);
      }
    },
    [getValues, setValue],
  );

  const tagIds = watch("tagIds");

  return (
    <form
      onSubmit={handleSubmit(onSubmit)}
      className="flex w-full flex-col gap-6"
      id="create-task"
    >
      <h2 className="text-xl font-semibold">Create a new task</h2>
      <Controller
        name="description"
        control={control}
        rules={{
          required: { value: true, message: "Description is required" },
        }}
        render={({ field, fieldState }) => (
          <Input
            label="Description"
            placeholder="What do I need to do?"
            fieldState={fieldState}
            {...field}
          />
        )}
      />

      <div className="flex flex-col gap-2">
        <span className="label">Select tags</span>
        <div className="flex flex-wrap gap-4">
          {tags && tags.length > 0 && (
            <ul className="flex flex-wrap gap-2">
              {tags.map((tag) => (
                <li key={tag.id}>
                  <button type="button" onClick={() => toggleTag(tag.id)}>
                    <TagLabel
                      tag={tag}
                      isActive={tagIds.includes(tag.id)}
                      showColorCircle
                    />
                  </button>
                </li>
              ))}
            </ul>
          )}
          <CreateTagDialog />
        </div>
      </div>
      <Button type="submit" className="self-end">
        Create
      </Button>
    </form>
  );
}
