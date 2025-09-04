import { Controller, SubmitHandler, useForm } from "react-hook-form";
import { createTag } from "wasp/client/operations";
import { Input } from "../../shared/components/Input";
import { ColorRadioButtons } from "./ColorRadioButtons";
import { generateBrightColor } from "./colors";
import { TagLabel } from "./TagLabel";

interface CreateTagFormProps {
  onTagCreated: () => void;
}

interface CreateTagFormValues {
  name: string;
  color: string;
}

export const CREATE_TAG_FORM_ID = "create-tag";

export function CreateTagForm({ onTagCreated }: CreateTagFormProps) {
  const { handleSubmit, setValue, watch, control, reset } =
    useForm<CreateTagFormValues>({
      defaultValues: {
        name: "",
        color: generateBrightColor(),
      },
    });

  const onSubmit: SubmitHandler<CreateTagFormValues> = async (data) => {
    try {
      await createTag(data);
      onTagCreated();
    } catch (err: unknown) {
      window.alert(`Error while creating tag: ${String(err)}`);
    } finally {
      reset();
    }
  };

  const [name, color] = watch(["name", "color"]);

  return (
    <form
      id={CREATE_TAG_FORM_ID}
      onSubmit={stopPropagate(handleSubmit(onSubmit))}
      className="flex flex-col gap-6"
    >
      <Controller
        name="name"
        control={control}
        rules={{
          required: { value: true, message: "Name is required" },
        }}
        render={({ field, fieldState }) => (
          <Input
            label="Name"
            placeholder="Enter tag name"
            fieldState={fieldState}
            {...field}
          />
        )}
      />
      <ColorRadioButtons
        color={color}
        setColor={(color) => setValue("color", color)}
      />
      {name && (
        <div className="flex flex-col gap-2">
          <span className="label">Preview</span>
          <div className="flex flex-wrap gap-2">
            <TagLabel
              tag={{ id: "", name, color }}
              isActive={true}
              showColorCircle
            />
            <TagLabel
              tag={{ id: "", name, color }}
              isActive={false}
              showColorCircle
            />
          </div>
        </div>
      )}
    </form>
  );
}

/**
 * Calling `stopPropagation()` on `SubmitHandler`'s event does not stop the propagation properly.
 * So we use this wrapper instead.
 *
 * @see https://github.com/react-hook-form/documentation/issues/916
 */
function stopPropagate(
  callback: (event: React.FormEvent<HTMLFormElement>) => void,
) {
  return (e: React.FormEvent<HTMLFormElement>) => {
    e.stopPropagation();
    callback(e);
  };
}
